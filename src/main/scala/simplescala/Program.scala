package simplescala

import core.Fixed.Config

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

trait Program extends Algebra {

  import Console._
  import Logging._
  import core.Fixed.CurrencyApi._
  import core.Fixed._

  def program(config: Config): Future[Unit] = {

    case class Transaction(fromCurrency: String, toCurrency: String, amount: BigDecimal, rate: BigDecimal)

    def singleExchange(fromCurrency: String,
                       toCurrency: String,
                       amount: BigDecimal): Future[Option[Transaction]] =
      (exchangeRate(fromCurrency, toCurrency)(config) map {
        case Right(rate) ⇒
          tell(s"$fromCurrency to $toCurrency rate = $rate")
          Some(Transaction(fromCurrency, toCurrency, amount, rate))
        case Left(error) ⇒
          logError(s"Couldn't fetch exchange rate. Error = $error")
          None
      }).recoverWith {
        case ex: Exception =>
          logError(s"Couldn't fetch error rate. Error = ${ex.getMessage}")
          tell(s"Do you want to retry? Enter Y/N.")
          if (safeAsk(identity).toLowerCase == "y")
            singleExchange(fromCurrency, toCurrency, amount)
          else Future.successful(None)
      }

    def exchangeLoop(currencyIdMap: Map[Int, String])(allTransactions: List[Transaction]): Future[Unit] = {
      val currencyIdsFormatted =
        currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",")
      tell(
        s"Choose a currency you want to convert from - $currencyIdsFormatted"
      )
      val fromCurrency = safeAsk(value ⇒ currencyIdMap(value.toInt))
      tell(s"You chose $fromCurrency")
      tell(s"How much you want to convert?")
      val amount = safeAsk(value ⇒ BigDecimal(value))
      tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
      val toCurrency = safeAsk(value ⇒ currencyIdMap(value.toInt))
      tell(s"You chose $toCurrency")
      singleExchange(fromCurrency, toCurrency, amount)
        .map(_.foldLeft(allTransactions)(_ :+ _))
        .map { transactions =>
          tell(transactions.map(t => s"Converted ${t.fromCurrency} ${t.amount} to ${t.toCurrency} at rate ${t.rate}").mkString("\n"))
          transactions
        }
        .flatMap(exchangeLoop(currencyIdMap))
    }

    allCurrencies(config)
      .flatMap { currencies =>
        val currencyIdMap = currencies.zipWithIndex
          .map(_.swap)
          .map(pair => (pair._1 + 1, pair._2))
          .toMap
        exchangeLoop(currencyIdMap)(List.empty)
      }
      .recoverWith {
        case ex: Exception =>
          logWarn(
            s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}"
          )
          program(config)
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
    def logInfo(msg: String): Unit = tell(s"Info -  $msg")
    def logError(msg: String): Unit = tell(s"Error -  $msg")
    def logWarn(msg: String): Unit = tell(s"Warn -  $msg")
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
  Await.result(program(Config()), Duration.Inf)
}
