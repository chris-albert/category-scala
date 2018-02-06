package io.lbert.category

import language.higherKinds

import io.lbert.category.Category.{Applicative, Box, Functor, Monoid}

object Implicits {

  object FunctorImplicits {

    implicit class FunctorInfix[F[_],A,B](af: A => B)(implicit fun: Functor[F]) {
      def infixMap(fa: F[A]): F[B] = fun.map(fa)(af)
      //Haskell alias
      def `<$>`(fa: F[A]): F[B] = infixMap(fa)
    }

    implicit val boxFunctor: Functor[Box] = new Functor[Box] {
      override def map[A, B](fa: Box[A])
                            (f: (A) => B): Box[B] = Box(f(fa.value))
    }

    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa.map(f)
    }
  }

  object ApplicativeImplicits {

    implicit class ApplicativeFuncInfix[F[_],A,B](a: F[A => B])
                                                 (implicit app: Applicative[F]) {
      def infixApply(fa: F[A]): F[B] = app.applyR(a)(fa)
      //Haskell alias
      def `<*>`(fa: F[A]): F[B] = infixApply(fa)
    }

    implicit val boxApplicative: Applicative[Box] = new Applicative[Box] {
      override def apply[A, B](fa: Box[A])(f: Box[A => B]): Box[B] = Box(f.value(fa.value))

      override def pure[A](a: => A): Box[A] = Box(a)

      override def map[A, B](fa: Box[A])(f: (A) => B): Box[B] = Box(f(fa.value))
    }

    val optionApplicative: Applicative[Option] = new Applicative[Option] {
      override def apply[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = (fa,f) match {
        case (Some(s),Some(fs)) => Some(fs(s))
        case _ => None
      }

      override def pure[A](a: => A): Option[A] = Some(a)

      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa.map(f)
    }
  }

  object MonoidImplicits {

    implicit def listMonoid[A] = new Monoid[List[A]] {
      override def zero: List[A] = List.empty[A]

      override def append(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    }
  }
}
