package chap1

import chap1.ProgramWithSideEffects.MyExperiment.Coffees

object ProgramWithSideEffects {

  object Cafe {

    trait CreditCard {
      def charge(price: Int): Unit = ()
    }

    trait Coffee {
      def price: Int = 100
    }

    def buyCoffee(cc: CreditCard): Coffee = {
      val cup = new Coffee {}
      cc.charge(cup.price) // side effect, possibly communicating over HTTP, does not compose

      cup
    }

  }

  object SlightlyBetterCafe {

    trait CreditCard

    trait Payments {
      def charge(cc: CreditCard, price: Int): Unit = ()
    }

    trait Coffee {
      def price: Int = 100
    }

    def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
      val cup = new Coffee {}
      p.charge(cc, cup.price) // side effect, but at least we gain testability, Payments can be mocked in tests, but not ideal

      cup
    }
  }

  object FunctionalCafe {

    trait CreditCard
    trait Coffee { def price: BigDecimal = 100 }
    final case class Charge(creditCard: CreditCard, price: BigDecimal) {
      def combine(other: Charge): Charge =
        if (creditCard == other.creditCard) Charge(creditCard, price + other.price)
        else throw new RuntimeException("Can not combine with someone else's cretid card")

    }

    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
      val cup = new Coffee {}

      // merely builds a data structure
      (cup, Charge(cc, cup.price)) // separating concerns of creating charge vs processing them, composable (used in buyCoffees)
    }

    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
      val (coffees, charges) = purchases.unzip

      (coffees, charges.reduce(_ combine _))
    }

    // Combine charges for the same person so that he/she pays only once
    def coalesce(charges: List[Charge]): List[Charge] =
      charges.groupBy(_.creditCard) // Map[CreditCard, List[Charge]]
        .values // Iterable[List[Charge]]
        .map(_.reduce(_ combine _)) // Iterable[Charge]
        .toList // List[Charge]

  }

  object MyExperiment {

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]: Functor]: Functor[F] = implicitly
    }

    implicit class FunctorSyntax[F[_]: Functor, A](self: F[A]) {
      def map[B](f: A => B): F[B] = Functor[F].map(self)(f)
    }

    trait Applicative[F[_]] extends Functor[F] {
      def pure[A](a: A): F[A]
    }

    object Applicative {
      def apply[F[_]: Applicative]: Applicative[F] = implicitly
    }

    implicit class ApplicativeSyntax[A](self: A) {
      def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(self)
    }

    implicit class MonadSyntax[F[_]: Monad, A](self: F[A]) {
      def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(self)(f)
    }

    trait Monad[F[_]] extends Applicative[F] {
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    }

    object Monad {
      def apply[F[_]: Monad]: Monad[F] = implicitly
    }

    trait CreditCard
    final case class Charge(creditCard: CreditCard, price: BigDecimal)
    final case class Coffee(price: BigDecimal)

    // algebra
    trait Coffees[F[_]] {
      def buy(creditCard: CreditCard): F[(Charge, CreditCard)]
      def buyMany(creditCard: CreditCard, n: Int): F[(List[Charge], CreditCard)]
    }

    object Coffees {
      def apply[F[_]: Coffees]: Coffees[F] = implicitly

      def of[F[_]: Monad]: Coffees[F] = new Coffees[F] {
        override def buy(creditCard: CreditCard): F[(Charge, CreditCard)] = {
          for {
            coffee <- Coffee(100).pure[F]
          } yield Charge(creditCard, coffee.price)
        }

        override def buyMany(creditCard: CreditCard, n: Int): F[(List[Charge], CreditCard)] = {
          for {
            purchases <- List.fill(n)(buy(creditCard)).pure[F]
            // here it needs Traverse because it's List[F[(Charge, CreditCard)]
            // and we want F[List[(Charge, CreditCard)]]
            // so it's kinda overkill :D but was fun to try out
            // after we use Traverse we can do what we did on line 66-68 but in F[_] way
          } yield ???
        }
      }

    }

    final case class IO[A](unsafeRun: () => A) {
      def map[B](f: A => B): IO[B] = IO.delay(f(unsafeRun()))

      def flatMap[B](f: A => IO[B]): IO[B] = f(unsafeRun())
    }

    object IO {
      def delay[A](a: => A): IO[A] = new IO[A](() => a)
      def pure[A](a: A): IO[A] = delay(a)
    }

    implicit object IOMonad extends Monad[IO] {
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
      override def pure[A](a: A): IO[A] = IO.pure(a)
      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    }

  }

  import MyExperiment._

  def main(args: Array[String]): Unit = {

    val io = Coffees.of[IO].buy(new CreditCard {})
    println(io.unsafeRun())
  }

}
