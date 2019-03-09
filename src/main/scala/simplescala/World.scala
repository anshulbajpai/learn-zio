package simplescala

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Random, Try}

object Program extends App with World {
  Await.result(program(Config()), Duration.Inf)
}

trait World {

  import Console._
  import CurrencyApi._
  import Logging._

  def program(config: Config): Future[Unit] = {

    val allExchanges = mutable.Buffer[String]()

    def singleExchange(fromCurrency: String,
                       toCurrency: String,
                       amount: BigDecimal): Future[Unit] =
      (exchangeRate(fromCurrency, toCurrency, config) map { rate =>
        tell(s"$fromCurrency to $toCurrency rate = $rate")
        allExchanges += s"Converted $fromCurrency $amount to $toCurrency at rate $rate"
        ()
      }).recoverWith {
        case ex: Exception =>
          error(s"Couldn't fetch error rate. Error = ${ex.getMessage}")
          tell(s"Do you want to retry? Enter Y/N.")
          if (safeAsk(identity).toLowerCase == "y")
            singleExchange(fromCurrency, toCurrency, amount)
          else Future.successful(())
      }

    def exchangeLoop(currencyIdMap: Map[Int, String]): Future[Unit] = {
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
        .map(_ => tell(allExchanges.mkString("\n")))
        .flatMap(_ ⇒ exchangeLoop(currencyIdMap))
    }

    allCurrencies(config)
      .flatMap { currencies =>
        val currencyIdMap = currencies.zipWithIndex
          .map(_.swap)
          .map(pair => (pair._1 + 1, pair._2))
          .toMap
        exchangeLoop(currencyIdMap)
      }
      .recoverWith {
        case ex: Exception =>
          warn(
            s"Couldn't fetch currencies. Re-fetching again. Error = ${ex.getMessage}"
          )
          program(config)
      }
  }

  def safeAsk[T](convert: String => T): T =
    Try(convert(ask)).fold({ ex =>
      info(s"User entered wrong input - ${ex.getMessage}")
      tell("Wrong input. Please enter again.")
      safeAsk(convert)
    }, identity)

  case class Config()

  object CurrencyApi {
    def allCurrencies(config: Config): Future[Set[String]] =
      randomize(Set("USD", "GBP", "INR", "SGD"), "Couldn't fetch currencies")

    def exchangeRate(from: String,
                     to: String,
                     config: Config): Future[BigDecimal] =
      randomize(
        BigDecimal(Random.nextInt(10000)) / 100,
        "Couldn't fetch exchange rate"
      )

    private def randomize[A](output: => A, error: => String) =
      if (Random.nextBoolean()) Future.successful(output)
      else Future.failed(new RuntimeException(error))
  }

  object Console {
    def tell(statement: String): Unit = println(statement)

    def ask: String = scala.io.StdIn.readLine()
  }

  object Logging {
    def info(msg: String): Unit = tell(s"Info -  $msg")

    def error(msg: String): Unit = tell(s"Error -  $msg")

    def warn(msg: String): Unit = tell(s"Warn -  $msg")
  }

}
