package usingIO

import cats.data._
import cats.effect.IO
import cats.instances.all._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.flatMap._

import scala.concurrent.Future
import scala.util.{Random, Try}

object Program extends App {
  program.run(Config()).unsafeRunAsyncAndForget()

  import Console._
  import CurrencyApi._
  import Logging._

  private type Exchanges[A] = State[List[String], A]
  private type FromConfig[A] = ReaderT[IO, Config, A]

  private lazy val program: FromConfig[Unit] = {

    case class TransactionRequest(fromCurrency: String, toCurrency: String, amount: BigDecimal)

    def createExchange(transactionRequest: TransactionRequest, errorOrRate: ErrorOr[BigDecimal]): IO[Exchanges[Unit]] = (for {
      rate <- IO.fromEither(errorOrRate)
      _ <- tell(s"${transactionRequest.fromCurrency} to ${transactionRequest.toCurrency} rate = $rate")
      exchanges <- addExchange(s"Converted ${transactionRequest.fromCurrency} ${transactionRequest.amount} to ${transactionRequest.toCurrency} at rate $rate")
    } yield exchanges).handleErrorWith { error =>
      logError(s"Couldn't fetch exchange rate. Error = $error") >> IO.pure(State.empty[List[String], Unit])
    }

    def handleTransaction(transactionRequest: TransactionRequest): FromConfig[Exchanges[Unit]] =
      exchangeRate(transactionRequest.fromCurrency, transactionRequest.toCurrency).mapF(futureToIo(_)).mapF { errorOrRateIO =>
        for {
          errorOrRate <- errorOrRateIO
          exchanges <- createExchange(transactionRequest, errorOrRate)
        } yield exchanges
      }.handleErrorWith { ex: Throwable =>
        val askForRetry = ioToConfigReader(logError(s"Couldn't fetch error rate. Error = ${ex.getMessage}") >>
          tell(s"Do you want to retry? Enter Y/N.") >>
          safeAsk(_.toLowerCase == "y"))
        for {
          canRetry <- askForRetry
          result <- if (canRetry) handleTransaction(transactionRequest) else ioToConfigReader(emptyExchanges)
        } yield result
      }

    def exchangeLoop(currencyIdMap: Map[Int, String])(oldExchanges: Exchanges[Unit]): FromConfig[Unit] = {
      val currencyIdsFormatted = currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",")

      val transactionRequest = for {
        _ <- tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
        fromCurrency <- safeAsk(value ⇒ currencyIdMap(value.toInt))
        _ <- tell(s"You chose $fromCurrency")
        _ <- tell(s"How much you want to convert?")
        amount <- safeAsk(value ⇒ BigDecimal(value))
        _ <- tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
        toCurrency <- safeAsk(value ⇒ currencyIdMap(value.toInt))
        _ <- tell(s"You chose $toCurrency")
      } yield TransactionRequest(fromCurrency, toCurrency, amount)

      (ioToConfigReader(transactionRequest) >>= handleTransaction).mapF { exchangesIO =>
        for {
          exchanges <- exchangesIO
          newExchanges = oldExchanges >> exchanges
          _ <- tell(newExchanges.runEmptyS.value.mkString("\n"))
        } yield newExchanges
      } >>= exchangeLoop(currencyIdMap)
    }

    allCurrencies.mapF(futureToIo(_))
      .flatMap { currencies =>
        val currencyIdMap = currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap
        exchangeLoop(currencyIdMap)(State.empty[List[String], Unit])
      }
      .handleErrorWith {
        case ex: Exception =>
          ioToConfigReader(logWarn(s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}")) >> program
      }
  }

  private def ioToConfigReader[A](io: IO[A]): FromConfig[A] = ReaderT(_ => io)

  private def futureToIo[A](f: => Future[A]): IO[A] = IO.fromFuture(IO(f))

  private def addExchange(exchangeMessage: String): IO[Exchanges[Unit]] = IO.pure(State(current => (current :+ exchangeMessage, Unit)))

  private def emptyExchanges = IO.pure(State.empty[List[String], Unit])

  private def safeAsk[T](convert: String => T): IO[T] =
    (for {
      value <- ask
      converted <- IO.fromEither(Try(convert(value)).toEither)
    } yield converted).handleErrorWith { ex: Throwable =>
      logInfo(s"User entered wrong input - ${ex.getMessage}") >>
        tell("Wrong input. Please enter again.") >>
        safeAsk(convert)
    }

  private case class Config()

  private object CurrencyApi {

    sealed trait Error extends Throwable

    case object WrongCurrency extends Error

    type ErrorOr[A] = Either[Error, A]

    def allCurrencies: ReaderT[Future, Config, Set[String]] = Kleisli(_ ⇒
      randomizeFuture(
        Set("USD", "GBP", "INR", "SGD"),
        "Couldn't fetch currencies"
      )
    )

    def exchangeRate(from: String, to: String): ReaderT[Future, Config, ErrorOr[BigDecimal]] = Kleisli(_ ⇒
      if (from == "SGD") Future.successful(WrongCurrency.asLeft[BigDecimal])
      else
        randomizeFuture(
          (BigDecimal(Random.nextInt(10000)) / 100).asRight[Error],
          "Couldn't fetch exchange rate"
        )
    )

    private def randomizeFuture[A](output: => A, error: => String) =
      if (Random.nextBoolean()) Future.successful(output)
      else Future.failed(new RuntimeException(error))
  }

  private object Console {
    def tell(statement: String): IO[Unit] = IO(println(statement))

    def ask: IO[String] = IO(scala.io.StdIn.readLine())
  }

  private object Logging {
    def logInfo(msg: String): IO[Unit] = log(s"Info -  $msg")

    def logError(msg: String): IO[Unit] = log(s"Error -  $msg")

    def logWarn(msg: String): IO[Unit] = log(s"Warn -  $msg")

    private def log(msg: String) = tell(msg)
  }

}
