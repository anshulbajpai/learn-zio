package finaltagless


import cats.MonadError
import cats.data.{ReaderT, StateT}
import cats.mtl.{ApplicativeAsk, MonadState}
import finaltagless.Fixed.{Config, CurrencyApi}

import scala.concurrent.Future
import scala.util.{Random, Try}

object Application extends App with Program {

  import cats.effect.IO
  import cats.effect.LiftIO._
  import cats.implicits._
  import cats.mtl.instances.all._

  type Effect1[A] = StateT[IO, List[String], A]
  type Effect[A] = ReaderT[Effect1, Config, A]

  program[Effect].run(Config()).foreverM.runEmptyS.unsafeRunAsyncAndForget()

  private implicit def consoleEffect(implicit consoleIO: Console[IO]): Application.Console[Effect] = new Console[Effect] {
    override def tell(statement: String): Effect[Unit] = consoleIO.tell(statement).to[Effect]

    override def ask: Effect[String] = consoleIO.ask.to[Effect]
  }

  private implicit def apiEffect(implicit apiIO: CurrencyApiF[IO]): Application.CurrencyApiF[Effect] = new CurrencyApiF[Effect] {
    override def allCurrencies(config: Config): Effect[Set[String]] = apiIO.allCurrencies(config).to[Effect]

    override def exchangeRate(from: String, to: String)(config: Config): Effect[BigDecimal] = apiIO.exchangeRate(from, to)(config).to[Effect]
  }

  private implicit lazy val ioConsole: Application.Console[IO] = new Console[IO] {
    override def tell(statement: String): IO[Unit] = IO(println(statement))

    override def ask: IO[String] = IO(scala.io.StdIn.readLine())
  }

  private implicit lazy val ioApi: Application.CurrencyApiF[IO] = new CurrencyApiF[IO] {
    override def allCurrencies(config: Config): IO[Set[String]] = IO.fromFuture(IO(CurrencyApi.allCurrencies(config)))

    override def exchangeRate(from: String, to: String)(config: Config): IO[BigDecimal] = IO.fromFuture(IO(CurrencyApi.exchangeRate(from, to)(config)))
  }
}

trait Program extends Algebra {

  import cats.implicits._

  def program[F[_] : ConfigAsk : CurrencyApiF : Console : ThrowableMonad : ExchangeState] = for {
    config <- config[F]
    currencies <- CurrencyApiF[F].allCurrencies(config)
    currencyIdMap = currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap
    currencyIdsFormatted = currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",")
    _ <- Console[F].tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
    fromCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- Console[F].tell(s"You chose $fromCurrency")
    _ <- Console[F].tell(s"How much you want to convert?")
    amount <- safeAsk(BigDecimal(_))
    _ <- Console[F].tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
    toCurrency <- safeAsk(value => currencyIdMap(value.toInt))
    _ <- Console[F].tell(s"You chose $toCurrency")
    rate <- CurrencyApiF[F].exchangeRate(fromCurrency, toCurrency)(config)
    _ <- Console[F].tell(s"$fromCurrency to $toCurrency rate = $rate")
    _ <- addExchange(s"Converted $fromCurrency $amount to $toCurrency at rate $rate")
    exchanges <- allExchanges
    _ <- Console[F].tell(exchanges.mkString("\n"))
  } yield ()

}

trait Algebra {

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.apply._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def config[F[_] : ConfigAsk]: F[Config] = ApplicativeAsk[F, Config].reader(identity)

  def addExchange[F[_] : ExchangeState](exchange: String): F[Unit] = MonadState[F, List[String]].modify(_ :+ exchange)

  def allExchanges[F[_] : ExchangeState]: F[List[String]] = MonadState[F, List[String]].inspect(identity)

  type ExchangeState[F[_]] = MonadState[F, List[String]]
  type ConfigAsk[F[_]] = ApplicativeAsk[F, Config]


  type ThrowableMonad[F[_]] = MonadError[F, Throwable]

  def safeAsk[F[_] : Console : ThrowableMonad, T](convert: String => T): F[T] =
    Console[F].ask
      .map(input => Try(convert(input)))
      .flatMap(_.fold(_.raiseError[F, T], _.pure))
      .handleErrorWith { _: Throwable =>
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
    def allCurrencies(config: Config): F[Set[String]]

    def exchangeRate(from: String, to: String)(config: Config): F[BigDecimal]
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

}

object Fixed {

  case class Config()

  object CurrencyApi {

    sealed trait Error extends Throwable

    case object WrongCurrency extends Error

    type ErrorOr[A] = Either[Error, A]

    def allCurrencies(config: Config): Future[Set[String]] =
      randomizeFuture(
        Set("USD", "GBP", "INR", "SGD"),
        "Couldn't fetch currencies"
      )

    def exchangeRate(from: String, to: String)(config: Config): Future[BigDecimal] =
    //      if (from == "SGD") Future.successful(WrongCurrency.asLeft[BigDecimal])
    //      else
      randomizeFuture(
        BigDecimal(Random.nextInt(10000)) / 100,
        "Couldn't fetch exchange rate"
      )

    private def randomizeFuture[A](output: => A, error: => String) =
      if (true) Future.successful(output)
      else Future.failed(new RuntimeException(error))
  }

}