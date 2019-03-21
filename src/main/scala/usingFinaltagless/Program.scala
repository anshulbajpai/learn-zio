package usingFinaltagless

import cats.data.{ReaderT, StateT}
import cats.mtl.{ApplicativeAsk, MonadState}
import cats.{Monad, MonadError, Show}
import fixed.Fixed.{Config, CurrencyApi}
import fixed.Fixed.CurrencyApi.ErrorOr

import scala.util.Try

object Application extends App with Program {

  import cats.effect.IO
  import cats.effect.LiftIO._
  import cats.implicits._
  import cats.mtl.instances.all._

  type ConfigedIO[A] = ReaderT[IO, Config, A]
  type Effect1[A] = StateT[IO, List[Transaction], A]
  type Effect[A] = ReaderT[Effect1, Config, A]

  program[Effect].run(Config()).runEmptyS.unsafeRunAsyncAndForget()

  private implicit def effectConsole(implicit ioConsole: Console[IO]): Application.Console[Effect] = new Console[Effect] {
    override def tell(statement: String): Effect[Unit] = ioConsole.tell(statement).to[Effect]
    override def ask: Effect[String] = ioConsole.ask.to[Effect]
  }

  private implicit def effectApi(implicit ioApi: CurrencyApiF[ConfigedIO]): Application.CurrencyApiF[Effect] = new CurrencyApiF[Effect] {
    override def allCurrencies: Effect[Set[String]] = ioApi.allCurrencies.mapF(wrapIOInState)
    override def exchangeRate(from: String, to: String): Effect[ErrorOr[BigDecimal]] = ioApi.exchangeRate(from, to).mapF(wrapIOInState)
    private def wrapIOInState[A, B](io: IO[A]): StateT[IO, B, A] = StateT(s => io.map(a => (s, a)))
  }

  private implicit def consoleLoggingEffect(implicit consoleLoggingIO: ConsoleLogging[IO]): Application.ConsoleLogging[Effect] = new ConsoleLogging[Effect]()

  private implicit lazy val ioConsole: Application.Console[IO] = new Console[IO] {
    override def tell(statement: String): IO[Unit] = IO(println(statement))
    override def ask: IO[String] = IO(scala.io.StdIn.readLine())
  }

  private implicit lazy val ioApi: Application.CurrencyApiF[ConfigedIO] = new CurrencyApiF[ConfigedIO] {
    override def allCurrencies = ReaderT(config => IO.fromFuture(IO(CurrencyApi.allCurrencies(config))))
    override def exchangeRate(from: String, to: String) = ReaderT(config => IO.fromFuture(IO(CurrencyApi.exchangeRate(from, to)(config))))
  }

  private implicit lazy val ioConsoleLogging: Application.ConsoleLogging[IO] = new ConsoleLogging[IO]
}

trait Program extends Algebra {

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.apply._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.option._

  def program[F[_] : ConfigAsk : CurrencyApiF : Console : ThrowableMonad : TransactionState : ConsoleLogging]: F[Unit] = for {
    currencies <- currencies
    currencyIdMap <- currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap.pure[F]
    _ <- transaction(currencyIdMap).handleErrorWith(handleTransactionError(_)).foreverM[Unit]
  } yield ()

  private def handleTransactionError[F[_] : ConsoleLogging](ex: Throwable): F[Unit] = ConsoleLogging[F].logError(ex.getMessage)

  private def transaction[F[_] : CurrencyApiF : Console : ThrowableMonad : ConsoleLogging : TransactionState](currencyIdMap: Map[Int, String]): F[Unit] = for {
    currencyIdsFormatted <- currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",").pure[F]
    _ <- Console[F].tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
    fromCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- Console[F].tell(s"You chose $fromCurrency")
    _ <- Console[F].tell(s"How much you want to convert?")
    amount <- safeAsk(BigDecimal(_))
    _ <- Console[F].tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
    toCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- Console[F].tell(s"You chose $toCurrency")
    rateOpt <- rate(fromCurrency, toCurrency)
    _ <- rateOpt.map(rate => handleTransaction(Transaction(fromCurrency, toCurrency, amount, rate))).getOrElse(().pure[F])
  } yield ()

  private def rate[F[_] : CurrencyApiF : ConsoleLogging : Console]
  (fromCurrency: String, toCurrency: String)
  (implicit throwableMonad: ThrowableMonad[F]): F[Option[BigDecimal]] = {
    userRetry(CurrencyApiF[F].exchangeRate(fromCurrency, toCurrency)).flatMap {
      case Some(Right(rate)) => rate.some.pure[F]
      case Some(Left(error)) => throwableMonad.raiseError(new RuntimeException(s"Couldn't fetch exchange rate. Error = $error"))
      case None => none[BigDecimal].pure[F]
    }
  }

  private def handleTransaction[F[_] : Console : TransactionState : Monad](transaction: Transaction): F[Unit] = for {
    _ <- Console[F].tell(s"${transaction.fromCurrency} to ${transaction.toCurrency} rate = ${transaction.rate}")
    _ <- addTransaction(transaction)
    transactions <- allTransactions
    _ <- Console[F].tell(transactions.map(Show[Transaction].show).mkString("\n"))
  } yield ()

  private def currencies[F[_] : CurrencyApiF : ThrowableMonad : ConsoleLogging]: F[Set[String]] =
    CurrencyApiF[F].allCurrencies
      .handleErrorWith { ex =>
        ConsoleLogging[F].logWarn(s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}") *> currencies
      }
}

trait Algebra {

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.apply._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.option._

  case class Transaction(fromCurrency: String, toCurrency: String, amount: BigDecimal, rate: BigDecimal)

  implicit val showTransaction: Show[Transaction] = new Show[Transaction] {
    override def show(transaction: Transaction): String =
      s"Converted ${transaction.fromCurrency} ${transaction.amount} to ${transaction.toCurrency} at rate ${transaction.rate}"
  }

  def config[F[_] : ConfigAsk]: F[Config] = ApplicativeAsk[F, Config].reader(identity)

  def addTransaction[F[_] : TransactionState](transaction: Transaction): F[Unit] = MonadState[F, List[Transaction]].modify(_ :+ transaction)
  def allTransactions[F[_] : TransactionState]: F[List[Transaction]] = MonadState[F, List[Transaction]].inspect(identity)

  type TransactionState[F[_]] = MonadState[F, List[Transaction]]
  type ConfigAsk[F[_]] = ApplicativeAsk[F, Config]

  type ThrowableMonad[F[_]] = MonadError[F, Throwable]

  def safeAsk[F[_] : Console : ThrowableMonad, T](convert: String => T): F[T] =
    Console[F].ask
      .map(input => Try(convert(input)))
      .flatMap(_.fold(_.raiseError[F, T], _.pure))
      .handleErrorWith { _ =>
        Console[F].tell("Wrong input. Please enter again.") *> safeAsk(convert)
      }


  trait Console[F[_]] {
    def tell(statement: String): F[Unit]
    def ask: F[String]
  }

  object Console {
    def apply[F[_] : Console] = implicitly[Console[F]]
  }

  trait CurrencyApiF[F[_]] {
    def allCurrencies: F[Set[String]]
    def exchangeRate(from: String, to: String): F[ErrorOr[BigDecimal]]
  }

  object CurrencyApiF {
    def apply[F[_] : CurrencyApiF] = implicitly[CurrencyApiF[F]]
  }

  trait Logging[F[_]] {
    def logInfo(msg: String): F[Unit]
    def logError(msg: String): F[Unit]
    def logWarn(msg: String): F[Unit]
  }

  class ConsoleLogging[F[_]](implicit consoleF: Console[F]) extends Logging[F] {
    def logInfo(msg: String): F[Unit] = log(s"Info -  $msg")
    def logError(msg: String): F[Unit] = log(s"Error -  $msg")
    def logWarn(msg: String): F[Unit] = log(s"Warn -  $msg")
    private def log(msg: String) = consoleF.tell(msg)
  }

  object ConsoleLogging {
    def apply[F[_] : ConsoleLogging] = implicitly[ConsoleLogging[F]]
  }

  def userRetry[F[_] : ThrowableMonad : ConsoleLogging : Console, A](fa: => F[A]): F[Option[A]] = fa.map(_.some).handleErrorWith { ex =>
    for {
      _ <- ConsoleLogging[F].logError(s"Got error. Error = ${ex.getMessage}")
      _ <- Console[F].tell("Do you want to retry? Enter y/n.")
      canRetry <- safeAsk(_.toLowerCase == "y")
      result <- if (canRetry) userRetry(fa) else none[A].pure[F]
    } yield result
  }
}

