package usingMonads

import cats.data._
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

  private type Exchanges[A] = State[List[String], A]
  type FromConfig[A] = ReaderT[Future, Config, A]

  def program: FromConfig[Unit] = {

    def addExchange(exchange: String): Exchanges[Unit] = State(current => (current :+ exchange, Unit))

    def singleExchange(fromCurrency: String,
                       toCurrency: String,
                       amount: BigDecimal): FromConfig[Exchanges[Unit]] =
      ReaderT((config: Config) => exchangeRate(fromCurrency, toCurrency)(config)).map {
        case Right(rate) =>
          tell(s"$fromCurrency to $toCurrency rate = $rate")
          addExchange(s"Converted $fromCurrency $amount to $toCurrency at rate $rate")
        case Left(error) ⇒
          logError(s"Couldn't fetch exchange rate. Error = $error")
          State.empty[List[String], Unit]
      }.handleErrorWith { ex: Throwable =>
        logError(s"Couldn't fetch error rate. Error = ${ex.getMessage}")
        tell(s"Do you want to retry? Enter Y/N.")
        if (safeAsk(identity).toLowerCase == "y")
          singleExchange(fromCurrency, toCurrency, amount)
        else ReaderT(_ => Future.successful(State.empty[List[String], Unit]))
      }

    def exchangeLoop(currencyIdMap: Map[Int, String])(oldExchanges: Exchanges[Unit]): FromConfig[Unit] = {
      val currencyIdsFormatted = currencyIdMap.map(pair => s"[${pair._1} - ${pair._2}]").mkString(",")
      tell(s"Choose a currency you want to convert from - $currencyIdsFormatted")
      val fromCurrency = safeAsk(value ⇒ currencyIdMap(value.toInt))
      tell(s"You chose $fromCurrency")
      tell(s"How much you want to convert?")
      val amount = safeAsk(value ⇒ BigDecimal(value))
      tell(s"Choose a currency want to convert to - $currencyIdsFormatted")
      val toCurrency = safeAsk(value ⇒ currencyIdMap(value.toInt))
      tell(s"You chose $toCurrency")
      singleExchange(fromCurrency, toCurrency, amount)
        .map { exchanges =>
          val newExchanges = oldExchanges.flatMap(_ => exchanges)
          tell(newExchanges.runEmptyS.value.mkString("\n"))
          newExchanges
        }
        .flatMap(exchangeLoop(currencyIdMap))
    }

    ReaderT((config: Config) => allCurrencies(config))
      .flatMap { currencies =>
        val currencyIdMap = currencies.zipWithIndex.map(_.swap).map(pair => (pair._1 + 1, pair._2)).toMap
        exchangeLoop(currencyIdMap)(State.empty[List[String], Unit])
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
