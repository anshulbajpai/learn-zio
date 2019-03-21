package usingIO

import cats.data.{State, _}
import cats.effect.IO
import cats.implicits._
import fixed.Fixed.Config
import fixed.Fixed.CurrencyApi.ErrorOr

import scala.concurrent.Future
import scala.util.Try
import fixed.Fixed.CurrencyApi._

trait Program extends Algebra {
  import Console._
  import Logging._

  val program: FromConfig[Unit] = {

    def createTransaction(fromCurrency: String,
                       toCurrency: String,
                       amount: BigDecimal, errorOrRate: ErrorOr[BigDecimal]): IO[TransactionState[Unit]] = (for {
      rate <- IO.fromEither(errorOrRate)
      _ <- tell(s"$fromCurrency to $toCurrency rate = $rate")
      transactions <- addTransaction(Transaction(fromCurrency, toCurrency, amount, rate))
    } yield transactions).handleErrorWith { error =>
      logError(s"Couldn't fetch exchange rate. Error = $error") *> IO.pure(noTransactions)
    }

    def handleTransaction(fromCurrency: String, toCurrency: String, amount: BigDecimal): FromConfig[TransactionState[Unit]] =
      ReaderT((config: Config) => exchangeRate(fromCurrency, toCurrency)(config)).mapF(_.toIO).mapF { errorOrRateIO =>
        for {
          errorOrRate <- errorOrRateIO
          transactions <- createTransaction(fromCurrency, toCurrency, amount, errorOrRate)
        } yield transactions
      }.handleErrorWith { ex: Throwable =>
        val askForRetry = logError(s"Couldn't fetch error rate. Error = ${ex.getMessage}") *>
          tell("Do you want to retry? Enter y/n.") *>
          safeAsk(_.toLowerCase == "y")
        for {
          canRetry <- askForRetry.toConfigReader
          result <- if (canRetry) handleTransaction(fromCurrency, toCurrency, amount) else IO.pure(noTransactions).toConfigReader
        } yield result
      }

    def convert(currencyIdMap: Map[Int, String]): FromConfig[TransactionState[Unit]] = {
      val currencyIdsFormatted = currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",")

      case class TransactionRequest(fromCurrency: String, toCurrency: String, amount: BigDecimal)

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

      transactionRequest.toConfigReader.flatMap(t => handleTransaction(t.fromCurrency, t.toCurrency, t.amount))
    }

    def conversions(currencyIdMap: Map[Int, String])(oldTransactions: TransactionState[Unit]): FromConfig[Unit] = convert(currencyIdMap)
      .map(newTransactions => oldTransactions *> newTransactions)
      .mapF(transactionsIO => transactionsIO.flatMap(transactions => tell(transactions.runEmptyS.value.map(t => s"Converted ${t.fromCurrency} ${t.amount} to ${t.toCurrency} at rate ${t.rate}").mkString("\n")) *> IO.pure(transactions)))
      .flatMap(conversions(currencyIdMap))

    ReaderT((config: Config) => allCurrencies(config)).mapF(_.toIO)
      .flatMap { currencies =>
        val currencyIdMap = currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap
        conversions(currencyIdMap)(noTransactions)
      }
      .handleErrorWith {
        case ex: Exception =>
          logWarn(s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}").toConfigReader *> program
      }
  }
}

trait Algebra {

  type FromConfig[A] = ReaderT[IO, Config, A]
  case class Transaction(fromCurrency: String, toCurrency: String, amount: BigDecimal, rate: BigDecimal)
  type TransactionState[A] = State[List[Transaction], A]

  implicit class IOOps[A](target: IO[A]) {
    val toConfigReader: FromConfig[A] = ReaderT(_ => target)
  }

  implicit class FutureOps[A](target: Future[A]) {
    val toIO: IO[A] = IO.fromFuture(IO(target))
  }

  def noTransactions = State.empty[List[Transaction], Unit]
  def addTransaction(transaction: Transaction): IO[TransactionState[Unit]] = IO.pure(State(current => (current :+ transaction, Unit)))

  object Console {
    def tell(statement: String): IO[Unit] = IO(println(statement))
    def ask: IO[String] = IO(scala.io.StdIn.readLine())
  }

  import Console._

  object Logging {
    def logInfo(msg: String): IO[Unit] = log(s"Info -  $msg")
    def logError(msg: String): IO[Unit] = log(s"Error -  $msg")
    def logWarn(msg: String): IO[Unit] = log(s"Warn -  $msg")
    private def log(msg: String) = tell(msg)
  }

  import Logging._

  def safeAsk[T](convert: String => T): IO[T] =
    (for {
      value <- ask
      converted <- IO.fromEither(Try(convert(value)).toEither)
    } yield converted).handleErrorWith { ex: Throwable =>
      logInfo(s"User entered wrong input - ${ex.getMessage}") *>
        tell("Wrong input. Please enter again.") *>
        safeAsk(convert)
    }
}

object Application extends App with Program {
  program.run(Config()).unsafeRunAsyncAndForget()
}
