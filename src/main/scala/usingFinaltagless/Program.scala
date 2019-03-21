package usingFinaltagless

import cats.data.{ReaderT, StateT}
import cats.mtl.{ApplicativeAsk, MonadState}
import cats.{Monad, MonadError, Show}
import fixed.Fixed.CurrencyApi.ErrorOr
import fixed.Fixed.{Config, CurrencyApi}

import scala.util.Try

trait Program extends Algebra {

  import Console._
  import ConsoleLogging._
  import CurrencyApiF._
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.apply._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.option._

  def program[F[_] : ConfigAsk : CurrencyApiF : Console : ThrowableMonad : TransactionState : ConsoleLogging]: F[Unit] = for {
    currencies <- currencies
    currencyIdMap <- currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap.pure[F]
    _ <- transaction(currencyIdMap).handleErrorWith(ex => logError(ex.getMessage)).foreverM[Unit]
  } yield ()

  private def transaction[F[_] : CurrencyApiF : Console : ThrowableMonad : ConsoleLogging : TransactionState](currencyIdMap: Map[Int, String]): F[Unit] = for {
    currencyIdsFormatted <- currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",").pure[F]
    _ <- tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
    fromCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- tell(s"You chose $fromCurrency")
    _ <- tell(s"How much you want to convert?")
    amount <- safeAsk(BigDecimal(_))
    _ <- tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
    toCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- tell(s"You chose $toCurrency")
    rateOpt <- rate(fromCurrency, toCurrency)
    _ <- rateOpt.map(rate => handleTransaction(Transaction(fromCurrency, toCurrency, amount, rate))).getOrElse(().pure[F])
  } yield ()

  private def rate[F[_] : CurrencyApiF : ConsoleLogging : Console]
  (fromCurrency: String, toCurrency: String)
  (implicit throwableMonad: ThrowableMonad[F]): F[Option[BigDecimal]] = {
    userRetry(exchangeRate(fromCurrency, toCurrency)).flatMap {
      case Some(Right(rate)) => rate.some.pure[F]
      case Some(Left(error)) => throwableMonad.raiseError(new RuntimeException(s"Couldn't fetch exchange rate. Error = $error"))
      case None => none[BigDecimal].pure[F]
    }
  }

  private def handleTransaction[F[_] : Console : TransactionState : Monad](transaction: Transaction): F[Unit] = for {
    _ <- tell(s"${transaction.fromCurrency} to ${transaction.toCurrency} rate = ${transaction.rate}")
    _ <- addTransaction(transaction)
    transactions <- allTransactions
    _ <- tell(transactions.map(Show[Transaction].show).mkString("\n"))
  } yield ()

  private def currencies[F[_] : CurrencyApiF : ThrowableMonad : ConsoleLogging]: F[Set[String]] =
    allCurrencies
      .handleErrorWith { ex =>
        logWarn(s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}") *> currencies
      }
}

trait Algebra {

  import Console._
  import ConsoleLogging._
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
    ask
      .map(input => Try(convert(input)))
      .flatMap(_.fold(_.raiseError[F, T], _.pure))
      .handleErrorWith(_ => tell("Wrong input. Please enter again.") *> safeAsk(convert))


  trait Console[F[_]] {
    def tell(statement: String): F[Unit]
    def ask: F[String]
  }

  object Console {
    def apply[F[_] : Console] = implicitly[Console[F]]
    def tell[F[_] : Console](statement: String): F[Unit] = Console[F].tell(statement)
    def ask[F[_] : Console]: F[String] = Console[F].ask
  }

  trait CurrencyApiF[F[_]] {
    def allCurrencies: F[Set[String]]
    def exchangeRate(from: String, to: String): F[ErrorOr[BigDecimal]]
  }

  object CurrencyApiF {
    def apply[F[_] : CurrencyApiF] = implicitly[CurrencyApiF[F]]
    def allCurrencies[F[_] : CurrencyApiF]: F[Set[String]] = CurrencyApiF[F].allCurrencies
    def exchangeRate[F[_] : CurrencyApiF](from: String, to: String): F[ErrorOr[BigDecimal]] = CurrencyApiF[F].exchangeRate(from, to)
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
    def logInfo[F[_] : ConsoleLogging](msg: String): F[Unit] = ConsoleLogging[F].logInfo(msg)
    def logError[F[_] : ConsoleLogging](msg: String): F[Unit] = ConsoleLogging[F].logError(msg)
    def logWarn[F[_] : ConsoleLogging](msg: String): F[Unit] = ConsoleLogging[F].logWarn(msg)
  }

  def userRetry[F[_] : ThrowableMonad : ConsoleLogging : Console, A](fa: => F[A]): F[Option[A]] = fa.map(_.some).handleErrorWith { ex =>
    for {
      _ <- logError(s"Got error. Error = ${ex.getMessage}")
      _ <- tell("Do you want to retry? Enter y/n.")
      canRetry <- safeAsk(_.toLowerCase == "y")
      result <- if (canRetry) userRetry(fa) else none[A].pure[F]
    } yield result
  }
}

trait EffectInstances {
  self: Algebra =>

  import cats.effect.IO
  import cats.effect.LiftIO._

  type ConfigedIO[A] = ReaderT[IO, Config, A]
  type Effect1[A] = StateT[IO, List[Transaction], A]
  type Effect[A] = ReaderT[Effect1, Config, A]

  implicit def effectConsole(implicit ioConsole: Console[IO]): Console[Effect] = new Console[Effect] {
    override def tell(statement: String): Effect[Unit] = ioConsole.tell(statement).to[Effect]
    override def ask: Effect[String] = ioConsole.ask.to[Effect]
  }

  implicit def effectApi(implicit ioApi: CurrencyApiF[ConfigedIO]): CurrencyApiF[Effect] = new CurrencyApiF[Effect] {
    override def allCurrencies: Effect[Set[String]] = ioApi.allCurrencies.mapF(wrapIOInState)
    override def exchangeRate(from: String, to: String): Effect[ErrorOr[BigDecimal]] = ioApi.exchangeRate(from, to).mapF(wrapIOInState)
    private def wrapIOInState[A, B](io: IO[A]): StateT[IO, B, A] = StateT(s => io.map(a => (s, a)))
  }

  implicit def consoleLoggingEffect(implicit consoleLoggingIO: ConsoleLogging[IO]): ConsoleLogging[Effect] = new ConsoleLogging[Effect]()

  implicit lazy val ioConsole: Console[IO] = new Console[IO] {
    override def tell(statement: String): IO[Unit] = IO(println(statement))
    override def ask: IO[String] = IO(scala.io.StdIn.readLine())
  }

  implicit lazy val ioApi: CurrencyApiF[ConfigedIO] = new CurrencyApiF[ConfigedIO] {
    override def allCurrencies = ReaderT(config => IO.fromFuture(IO(CurrencyApi.allCurrencies(config))))
    override def exchangeRate(from: String, to: String) = ReaderT(config => IO.fromFuture(IO(CurrencyApi.exchangeRate(from, to)(config))))
  }

  implicit lazy val ioConsoleLogging: ConsoleLogging[IO] = new ConsoleLogging[IO]
}

object Application extends App with Program with EffectInstances {
  import cats.implicits._
  import cats.mtl.instances.all._
  program[Effect].run(Config()).runEmptyS.unsafeRunAsyncAndForget()
}


