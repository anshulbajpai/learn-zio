package useZio

import cats.Show
import cats.syntax.option._
import fixed.Fixed.CurrencyApi.ErrorOr
import fixed.Fixed.{Config, CurrencyApi}
import scalaz.zio.{TaskR, UIO, ZIO}

import scala.util.Try

trait Program extends Algebra {

  import Console._
  import CurrencyApiZ._
  import Logging._

  val program = for {
    currencies <- autoRetry(allCurrencies)(ex => s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}")
    currencyIdMap = currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap
    _ <- transactionLoop(currencyIdMap)(Seq.empty)
  } yield ()

  private def transactionLoop(currencyIdMap: Map[Int, String])(oldTransactions: Seq[Transaction]): ZIO[ConfigProvider with CurrencyApiZ with Console with Logging, Nothing, Unit]
  = transaction(currencyIdMap)
    .map(_.foldLeft(oldTransactions)(_ :+ _))
    .flatMapError(ex => logError(ex.getMessage))
    .orElse(UIO.succeed(oldTransactions))
    .flatMap(transactions => tell(transactions.map(Show[Transaction].show).mkString("\n")) *> UIO.succeed(transactions))
    .flatMap(ts => transactionLoop(currencyIdMap)(ts))

  private def transaction(currencyIdMap: Map[Int, String]) = for {
    currencyIdsFormatted <- UIO.succeed(currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(","))
    _ <- tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
    fromCurrency <- safeAsk(v => currencyIdMap(v.toInt))
    _ <- tell(s"You chose $fromCurrency")
    _ <- tell(s"How much you want to convert?")
    amount <- safeAsk(BigDecimal(_))
    _ <- tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
    toCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- tell(s"You chose $toCurrency")
    rateOpt <- rate(fromCurrency, toCurrency)
    transaction = rateOpt.map(rate => Transaction(fromCurrency, toCurrency, amount, rate))
    _ <- transaction.map(t => tell(s"${t.fromCurrency} to ${t.toCurrency} rate = ${t.rate}")) .getOrElse(UIO.unit)
    transactionZ <- UIO.succeed(transaction)
  } yield transactionZ

  private def rate(fromCurrency: String, toCurrency: String) =
    userRetry(exchangeRate(fromCurrency, toCurrency))(ex => s"Couldn't fetch exchange rate - Error = ${ex.getMessage}").flatMap {
      case Some(Right(rate)) => UIO.succeed(rate.some)
      case None => UIO.succeed(none[BigDecimal])
      case Some(Left(error)) => ZIO.fail(new RuntimeException(s"Couldn't fetch rate - Error = ${error.toString}"))
    }

}

trait Algebra {

  import Console._
  import Logging._

  trait Console {
    def console: Console.Service
  }

  trait ConfigProvider {
    def service: ConfigProvider.Service
  }

  object ConfigProvider {

    trait Service {
      def config: Config
    }

    trait Live extends Service {
      val config = Config()
    }

    object Live extends Live

  }

  object Console {

    trait Service {
      val ask: UIO[String]
      def tell(value: String): UIO[Unit]
    }

    trait Live extends Service {
      override val ask: UIO[String] = UIO.effectTotal(scala.io.StdIn.readLine())
      override def tell(value: String): UIO[Unit] = UIO.effectTotal(println(value))
    }

    object Live extends Live

    val ask: ZIO[Console, Nothing, String] = ZIO.accessM(_.console.ask)
    def tell(value: String): ZIO[Console, Nothing, Unit] = ZIO.accessM(_.console.tell(value))

  }

  trait Logging {
    def logging: Logging.Service
  }

  object Logging {

    trait Service {
      def logInfo(msg: String): UIO[Unit]
      def logWarn(msg: String): UIO[Unit]
      def logError(msg: String): UIO[Unit]
    }

    trait Live extends Service with Console.Live {
      def logInfo(msg: String): UIO[Unit] = tell(s"Info - $msg")
      def logWarn(msg: String): UIO[Unit] = tell(s"Warn - $msg")
      def logError(msg: String): UIO[Unit] = tell(s"Error - $msg")
    }

    object Live extends Live

    def logInfo(msg: String): ZIO[Logging, Nothing, Unit] = ZIO.accessM(_.logging.logInfo(msg))
    def logWarn(msg: String): ZIO[Logging, Nothing, Unit] = ZIO.accessM(_.logging.logWarn(msg))
    def logError(msg: String): ZIO[Logging, Nothing, Unit] = ZIO.accessM(_.logging.logError(msg))

  }

  trait CurrencyApiZ {
    def currencyApi: CurrencyApiZ.Service
  }

  object CurrencyApiZ {

    trait Service {
      val allCurrencies: TaskR[ConfigProvider, Set[String]]
      def exchangeRate(from: String, to: String): TaskR[ConfigProvider, ErrorOr[BigDecimal]]
    }

    trait Live extends Service {
      val allCurrencies: TaskR[ConfigProvider, Set[String]] = ZIO.accessM(provider => ZIO.fromFuture(_ => CurrencyApi.allCurrencies(provider.service.config)))
      def exchangeRate(from: String, to: String): TaskR[ConfigProvider, ErrorOr[BigDecimal]] =
        ZIO.accessM(provider => ZIO.fromFuture(implicit ec => CurrencyApi.exchangeRate(from, to)(provider.service.config)))
    }

    object Live extends Live

    val allCurrencies: ZIO[ConfigProvider with CurrencyApiZ, Throwable, Set[String]] = ZIO.accessM(_.currencyApi.allCurrencies)
    def exchangeRate(from: String, to: String): ZIO[ConfigProvider with CurrencyApiZ, Throwable, ErrorOr[BigDecimal]] = ZIO.accessM(_.currencyApi.exchangeRate(from, to))

  }

  def autoRetry[R, E, A](zio: ZIO[R, E, A])(errorLog: E => String): ZIO[Logging with R, Nothing, A] =
    zio.flatMapError(e => logWarn(errorLog(e))).orElse(autoRetry(zio)(errorLog))

  def userRetry[R, E, A](zio: ZIO[R, E, A])(errorLog: E => String): ZIO[R with Console with Logging, Nothing, Option[A]] = {
    val retry = for {
      _ <- tell("Do you want to retry? Enter y/n.")
      canRetry <- safeAsk(_.toLowerCase == "y")
      result <- if (canRetry) userRetry(zio)(errorLog) else UIO.succeed(none[A])
    } yield result
    zio.map(_.some).flatMapError(e => logError(errorLog(e))).orElse(retry)
  }

  def safeAsk[T](convert: String => T): ZIO[Console with Logging, Nothing, T] = (for {
    stringValue <- ask
    value <- ZIO.fromTry(Try(convert(stringValue)))
  } yield value)
    .flatMapError(ex => logError(s"User entered wrong input - ${ex.getMessage}"))
    .orElse(tell("Wrong input. Please enter again.") *> safeAsk(convert))

  case class Transaction(fromCurrency: String, toCurrency: String, amount: BigDecimal, rate: BigDecimal)

  implicit val showTransaction: Show[Transaction] = new Show[Transaction] {
    override def show(t: Transaction): String = s"Converted ${t.fromCurrency} ${t.amount} to ${t.toCurrency} at rate ${t.rate}"
  }

}

object Application extends scalaz.zio.App with Program {
  val liveEnv = new Console with ConfigProvider with CurrencyApiZ with Logging {
    override val console: Application.Console.Service = Console.Live
    override val service: Application.ConfigProvider.Service = ConfigProvider.Live
    override val currencyApi: Application.CurrencyApiZ.Service = CurrencyApiZ.Live
    override val logging: Application.Logging.Service = Logging.Live
  }
  override def run(args: List[String]): ZIO[Application.Environment, Nothing, Int] = program.provide(liveEnv).fold(_ => 1, _ => 0)
}