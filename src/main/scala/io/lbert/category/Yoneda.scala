package io.lbert.category

import cats.Functor
import cats.syntax.functor._

trait Yoneda[F[_], A] { self =>
  def apply[B](f: A => B): F[B]
  def run: F[A] = apply(identity[A])
  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    override def apply[C](g: B => C): F[C] = self(f andThen g)
  }
}

object Yoneda {

  def apply[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] = {
    new Yoneda[F, A] {
      override def apply[B](f: A => B): F[B] = fa.map(f)
    }
  }
}
