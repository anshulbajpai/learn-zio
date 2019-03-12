package simplemonads

import cats.data._
import cats.instances.all._
import cats.syntax.applicativeError._
import cats.syntax.either._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Random, Try}

object Program extends App {
  Await.result(program.run(Config()), Duration.Inf)

  import Console._
  import CurrencyApi._
  import Logging._

  private type Exchanges[A] = State[List[String], A]
  private type FromConfig[A] = ReaderT[Future, Config, A]

  private def program: FromConfig[Unit] = {

    def addExchange(exchange: String): Exchanges[Unit] = State(current => (current :+ exchange, Unit))

    def singleExchange(fromCurrency: String,
                       toCurrency: String,
                       amount: BigDecimal): FromConfig[Exchanges[Unit]] =
      exchangeRate(fromCurrency, toCurrency).map {
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

    allCurrencies
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

  private def safeAsk[T](convert: String => T): T =
    Try(convert(ask)).fold({ ex =>
      logInfo(s"User entered wrong input - ${ex.getMessage}")
      tell("Wrong input. Please enter again.")
      safeAsk(convert)
    }, identity)

  private case class Config()

  private object CurrencyApi {

    sealed trait Error

    case object WrongCurrency extends Error

    type ErrorOr[A] = Either[Error, A]

    val allCurrencies: FromConfig[Set[String]] = Kleisli(_ ⇒
      randomizeFuture(
        Set("USD", "GBP", "INR", "SGD"),
        "Couldn't fetch currencies"
      )
    )

    def exchangeRate(from: String, to: String): FromConfig[ErrorOr[BigDecimal]] = Kleisli(_ ⇒
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
    def tell(statement: String): Unit = println(statement)

    def ask: String = scala.io.StdIn.readLine()
  }

  private object Logging {
    def logInfo(msg: String) = log(s"Info -  $msg")

    def logError(msg: String) = log(s"Error -  $msg")

    def logWarn(msg: String) = log(s"Warn -  $msg")

    private def log(msg: String) = tell(msg)
  }

}
