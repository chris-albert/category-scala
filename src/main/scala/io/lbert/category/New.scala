package io.lbert.category

import cats.data.Validated
import cats.syntax.validated._

object Groups {

  trait Magma[A] {
    def combine(f: A, s: A): A
  }

  /**
    * Associative axiom: (a . b) . c == a . (b . c)
    */
  trait Semigroup[A] extends Magma[A]

  /**
    * Identity axiom: a . id = id . a = a
    */
  trait Monoid[A] extends Semigroup[A] {
    def identity: A
  }

  /**
    * A group is any set of objects with an associated
    * operation that combines pairs of objects in the set.
    *
    * Inverse axiom: a . inv = b = inv . a
    */
  trait Group[A] extends Monoid[A] {
    def inverse: A
  }

  /**
    * Commutativity axiom: a . b = b . a
    */
  trait AbelianGroup[A] extends Group[A]
}

object Fields {

  trait Semiring[A]

  trait Ring[A] extends Semiring[A]

  trait IntegralDomain[A] extends Ring[A]

  trait Field[A] extends IntegralDomain[A]
}

trait Axioms[F[_]] {
  def check[A](f: F[A]): Validated[String, Unit]
}

object Axioms {
  def apply[F[_]](implicit A: Axioms[F]): Axioms[F] = A
}

trait Gen[A] {
  def generate: A
}

