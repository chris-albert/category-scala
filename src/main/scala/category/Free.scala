package category

import category.Category.{FreeMonad, Id, Monad}
import language.higherKinds

object Free {


  sealed trait FeatureA[A]
  case class Put[A](key: String , a: A) extends FeatureA[Unit]
  case class Get[A](key: String) extends FeatureA[Option[A]]

  type Feature[A] = FreeMonad[FeatureA, A]

  def put[A](key: String, a: A): Feature[Unit] =
    FreeMonad.liftF[FeatureA, Unit](Put[A](key, a))

  def get[A](key: String): Feature[Option[A]] =
    FreeMonad.liftF[FeatureA, Option[A]](Get[A](key))

  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  val interactListProgram = List(
    Ask("What's your first name?"),
    Ask("What's your last?"),
    Tell("Hello, ???")
  )

  sealed trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Bind(i,k) =>
          Bind(i, k andThen (_ flatMap f))
      }

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    def foldMap[G[_]: Monad](f: F ~> G): G[A] =
      this match {
        case Return(a) => Monad[G].pure(a)
        case Bind(fx, g) =>
          Monad[G].flatMap(f(fx)) { a =>
            g(a).foldMap(f)
          }
      }
  }

  /**
    * Free Functor     : Programs that change value
    * Free Applicatives: Programs that build data
    * - Codecs, Parsers
    * Free Monads      : Programs that build programs
    */

  object Free {
    def lift[F[_],A](fa: F[A]): Free[F,A] =
      Bind(fa, (a: A) => Return(a))
  }

  case class Return[F[_],A](a: A) extends Free[F,A]

  case class Bind[F[_],I,A](
    i: F[I],
    k: I => Free[F,A]
  ) extends Free[F,A]

  val interactMonadProgram = for {
    x <- Free.lift(Ask("What's your first name?"))
    y <- Free.lift(Ask("What's your last?"))
    _ <- Free.lift(Tell(s"Hello, $x $y"))
  } yield ()

  sealed trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  object Console extends (Interact ~> Id) {
    override def apply[A](i: Interact[A]): Id[A] = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine
      case Tell(msg) =>
        println(msg)
    }
  }

  sealed trait ExtractorA[A]

  case class Extract[A](k: String) extends ExtractorA[A]

  def freeTest = {
    val f = for {
      a <- Free.lift(Extract[Double]("a"))
      b <- Free.lift(Extract[Double]("b"))
      c <- Free.lift(Extract[Option[Double]]("c"))
    } yield a + b + c.getOrElse(0d)
  }

  case class Chain[A](k: String) {
    def map[B](f: A => B): Chain[B] = ???
    def flatMap[B](f: A => Chain[B]): Chain[B] = ???
  }

  def foo = {
    val f = for {
      a <- Chain[Double]("a")
      b <- Chain[Double]("b")
      c <- Chain[Option[Double]]("d")
    } yield a + b + c.getOrElse(0d)
  }

}
