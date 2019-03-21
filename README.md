# Learn-ZIO

This repo has code examples to understand the advantages of using [ZIO](https://scalaz.github.io/scalaz-zio/) library
by comparing with the code examples using other libraries and techniques

The order in which the code exmaples should be read is given below:-
1. `usingVanillascala` (using plain old scala with Futures and executing side-effects eagerly)
2. `usingMonads` (using Monads and MonadTransformers with Futures and executing side-effects eagerly)
3. `usingIO` (using [IO](https://typelevel.org/cats-effect/datatypes/io.html) and pushing side-effects to the boundary)
4. `usingFinaltagless` (using final tagless technique complimented with [mtl](https://typelevel.org/cats-mtl/))
5. `usingZIO` (to be implemented) 

The above mentioned terms represents different packages under source code.

There is a `fixed` package which contains the `Config` case class and `CurrencyAPI` which every code example uses.

Every code-example contains a `Program` trait which depends on `Algebra` trait. The executable `Application` executes the `program` defined in the `Program` trait. 
 
All the code examples are based on the following requirements

1. Fetch all currencies from API
2. Ask user from which currency they want to exchange
3. Ask how much they want to exchange
4. Ask to which currency they wants to exchange
5. Fetch the current exchange rate
6. Do the exchange
7. Maintain transaction state for the user's session
8. Do necessary exception handling and logging (e.g. when user input is wrong, the API returns error or throws exception)