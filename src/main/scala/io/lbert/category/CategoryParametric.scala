package io.lbert.category

import cats.data.Kleisli
import scala.language.higherKinds

object CategoryParametric {

  trait FunctorF[F[_]] {
    def map[A, B](f: A => B): F[A] => F[B]
  }

  trait Category[==>[_, _]] {
    def id[A]: A ==> A
    def compose[A, B, C](f: B ==> C, g: A ==> B): A ==> C
  }

  //The category of scal (scala types and functions)
  val scal: Category[Function1] = new Category[Function1] {
    override def id[A]: A => A = Predef.identity
    override def compose[A, B, C](f: B => C, g: A => B): A => C =
      f.compose(g)
  }

  def kleisli[M[_]](implicit M: cats.Monad[M]): Category[Kleisli[M, ?, ?]] =
    new Category[Kleisli[M, ?, ?]] {
      override def id[A]: Kleisli[M, A, A] = Kleisli(M.pure[A])
      override def compose[A, B, C](f: Kleisli[M, B, C], g: Kleisli[M, A, B]): Kleisli[M, A, C] =
        Kleisli(a => M.flatMap(g.run(a))(f.run))
    }

  trait EndoFunctor[==>[_, _], F[_]] {
    def map[A, B](f: A ==> B): F[A] ==> F[B]
  }

  //This is actually the regular functor
  trait ExoFunctor[==>[_, _], |==>[_, _], F[_]] {
    def map[A, B](f: A ==> B): F[A] |==> F[B]
  }

  type EndoFunctor2[==>[_, _], F[_]] = ExoFunctor[==> ,==> , F]

  type Functor2[F[_]] = EndoFunctor[Function1, F]

  type Traverse[M[_], F[_]] = EndoFunctor[Kleisli[M, ?, ?], F]
  //map[A, B](f => M[B]): F[A] => M[F[B]]

  type KleisliFunctor[M[_], F[_]] = ExoFunctor[Kleisli[M, ?, ?], Function1, F]

  type FunctorFilter[F[_]] = KleisliFunctor[Option, F]

  type FlatMap[F[_]] = KleisliFunctor[F, F]


  case class Op[==>[_, _], A, B](run: B => A)

  type Presheaf[==>[_, _], F[_]] = ExoFunctor[Op[==>, ?, ?], ==>, F]

  type Contravariant[F[_]] = Presheaf[Function1, F]

}
