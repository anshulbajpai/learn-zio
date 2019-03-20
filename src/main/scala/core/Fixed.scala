package core

import scala.concurrent.Future
import scala.util.Random

object Fixed {

  case class Config()

  object CurrencyApi {

    import cats.syntax.either._

    sealed trait Error extends Throwable
    case object WrongCurrency extends Error

    type ErrorOr[A] = Either[Error, A]

    def allCurrencies(config: Config): Future[Set[String]] =
      randomizeFuture(Set("USD", "GBP", "INR", "SGD"), "Couldn't fetch currencies")

    def exchangeRate(from: String, to: String)(config: Config): Future[ErrorOr[BigDecimal]] =
      if (from == "SGD") Future.successful(WrongCurrency.asLeft[BigDecimal])
      else
        randomizeFuture((BigDecimal(Random.nextInt(10000)) / 100).asRight[Error], "Couldn't fetch exchange rate")

    private def randomizeFuture[A](output: => A, error: => String) =
      if (Random.nextBoolean()) Future.successful(output)
      else Future.failed(new RuntimeException(error))
  }

}
