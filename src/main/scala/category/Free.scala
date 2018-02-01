package category

import category.Category.{FreeMonad, Id, Monad}

object Free {


  sealed trait FeatureA[A]
  case class Put[A](key: String , a: A) extends FeatureA[Unit]
  case class Get[A](key: String) extends FeatureA[Option[A]]

  type Feature[A] = FreeMonad[FeatureA, A]

  def put[A](key: String, a: A): Feature[Unit] =
    FreeMonad.liftF[FeatureA, Unit](Put[A](key, a))

  def get[A](key: String): Feature[Option[A]] =
    FreeMonad.liftF[FeatureA, Option[A]](Get[A](key))


  sealed trait ExtractorA[A]

  case class Extract[A,B](a: A) extends ExtractorA[B]

//  def chain[A](k: String): Monad[A] = ???
//
//  def foo = {
//    val f = for {
//      a <- chain[Double]("a")
//      b <- chain[Int]("b")
//      c <- chain[Boolean]("c")
//    } yield a + b + c
//  }

}
