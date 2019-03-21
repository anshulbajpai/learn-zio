package usingMonads

import cats.data.{State, _}
import cats.instances.all._
import cats.syntax.applicativeError._
import fixed.Fixed.Config
import fixed.Fixed.CurrencyApi._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

trait Program extends Algebra {

  import Console._
  import Logging._

  type FromConfig[A] = ReaderT[Future, Config, A]

  def program: FromConfig[Unit] = {

    type TransactionState[A] = State[List[Transaction], A]

    case class Transaction(fromCurrency: String, toCurrency: String, amount: BigDecimal, rate: BigDecimal)

    def addExchange(exchange: Transaction): TransactionState[Unit] = State(current => (current :+ exchange, Unit))

    def singleTransaction(fromCurrency: String,
                          toCurrency: String,
                          amount: BigDecimal): FromConfig[TransactionState[Unit]] =
      ReaderT((config: Config) => exchangeRate(fromCurrency, toCurrency)(config)).map {
        case Right(rate) =>
          tell(s"$fromCurrency to $toCurrency rate = $rate")
          addExchange(Transaction(fromCurrency, toCurrency, amount, rate))
        case Left(error) ⇒
          logError(s"Couldn't fetch exchange rate. Error = $error")
          State.empty[List[Transaction], Unit]
      }.handleErrorWith { ex: Throwable =>
        logError(s"Couldn't fetch error rate. Error = ${ex.getMessage}")
        tell(s"Do you want to retry? Enter Y/N.")
        if (safeAsk(identity).toLowerCase == "y")
          singleTransaction(fromCurrency, toCurrency, amount)
        else ReaderT(_ => Future.successful(State.empty[List[Transaction], Unit]))
      }

    import cats.syntax.apply._

    def transactionLoop(currencyIdMap: Map[Int, String])(oldTransactions: TransactionState[Unit]): FromConfig[Unit] = {
      val currencyIdsFormatted = currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",")
      tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
      val fromCurrency = safeAsk(value ⇒ currencyIdMap(value.toInt))
      tell(s"You chose $fromCurrency")
      tell(s"How much you want to convert?")
      val amount = safeAsk(value ⇒ BigDecimal(value))
      tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
      val toCurrency = safeAsk(value ⇒ currencyIdMap(value.toInt))
      tell(s"You chose $toCurrency")
      singleTransaction(fromCurrency, toCurrency, amount)
        .map { exchanges =>
          val newExchanges = oldTransactions *> exchanges
          tell(newExchanges.runEmptyS.value.map(t => s"Converted ${t.fromCurrency} ${t.amount} to ${t.toCurrency} at rate ${t.rate}").mkString("\n"))
          newExchanges
        }
        .flatMap(transactionLoop(currencyIdMap))
    }

    ReaderT((config: Config) => allCurrencies(config))
      .flatMap { currencies =>
        val currencyIdMap = currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap
        transactionLoop(currencyIdMap)(State.empty[List[Transaction], Unit])
      }
      .handleErrorWith {
        case ex: Exception =>
          logWarn(s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}")
          program
      }
  }

}

trait Algebra {

  object Console {
    def tell(statement: String): Unit = println(statement)
    def ask: String = scala.io.StdIn.readLine()
  }

  import Console._

  object Logging {
    def logInfo(msg: String) = log(s"Info -  $msg")
    def logError(msg: String) = log(s"Error -  $msg")
    def logWarn(msg: String) = log(s"Warn -  $msg")
    private def log(msg: String) = tell(msg)
  }

  import Logging._

  def safeAsk[T](convert: String => T): T =
    Try(convert(ask)).fold({ ex =>
      logInfo(s"User entered wrong input - ${ex.getMessage}")
      tell("Wrong input. Please enter again.")
      safeAsk(convert)
    }, identity)
}

object Application extends App with Program {
  Await.result(program.run(Config()), Duration.Inf)
}
