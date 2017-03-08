package category


object Category {

  case class Box[A](value: A)

  /**
    * Functor:
    *   Computational Context
    *   Mapping between categories or morphisms of categories
    *
    *   For something to be a Functor it needs to have a map method with the signature:
    *   F[A] => (f: A => B) => F[B]
    *
    *   Functor Laws:
    *   1: Identity
    *   2: Composition
    *``
    *   Composing 2 functors is also a functor
    */

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B] // In haskell this is fmap or infix <$>
    //lift is just params reversed map
    def lift[A, B](f: A => B)(fa: F[A]): F[B] = map(fa)(f)
  }

  object Functor {
    //This is a helper so you can do: Functor[Option].map(...)(...)
    def apply[F[_]](implicit f: Functor[F]): Functor[F] = f
  }

  /**
    * Applicative (Functor)
    *   Applicative's solve the problem of doing calculations on categories
    *
    *   F[A] => F[A => B] => F[B]
    *
    *   Applicative Laws:
    *   1. Identity
    *   2. Homomorphism
    *   3. Interchange
    *   4. Composition
    */

  trait Applicative[F[_]] extends Functor[F] {
    def apply[A, B](fa: F[A])(f: F[A => B]): F[B] //In haskell this is infix <*>
    //Same as apply but params reversed
    def applyR[A, B](f: F[A => B])(fa: F[A]): F[B] = apply(fa)(f)
    def pure[A](a: => A): F[A]
    //Same as pure, just diff name
    def point[A](a: => A): F[A] = pure[A](a)
  }

  object Applicative {
    def apply[F[_]](implicit a: Applicative[F]): Applicative[F] = a
  }

  /**
    * Monad:
    *   Control flow
    *   F[A] => (f: A => F[B]) => F[B]
    *
    */
  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A,B](fs: F[A])(f: A => F[B]): F[B] // In haskell this is bind or >>=
  }

  trait Semigroup[A] {
    def append(a1: A, a2: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  type Id[+A] = A

  /**
    * ReaderMonad
    *   Dependency injection for monad's
    */

  case class ReaderMonad[-A,+B](run: A => B) {
    def apply(a: A): B = run(a)
    def map[C](f: B => C): ReaderMonad[A,C] =
      ReaderMonad(a => f(run(a)))
    def flatMap[C <: A,D](f: B => ReaderMonad[C,D]): ReaderMonad[C,D] =
      ReaderMonad(c => f(run(c))(c))
  }

  case class WriterMonad[A,B](value: A, log: B)(implicit m: Monoid[B]) {
    def map[C](f: A => C): WriterMonad[C,B] = WriterMonad(f(value),log)
    def flatMap[C](f: A => WriterMonad[C,B]): WriterMonad[C,B] = f(value) match {
      case WriterMonad(v,l) => WriterMonad(v,m.append(l,log))
    }
  }

  /**
    * Free Monad:
    *   Brings together monads and interpreters
    *   
    *   Allows us to abstractly specify control flow between pure functions,
    *   and separately define an implementation
    */

  trait FreeMonad[F[_],A]
  case class Point[F[_],A](a: A) extends FreeMonad[F,A]
  case class Join[F[_],A](s: F[FreeMonad[F,A]]) extends FreeMonad[F,A]
  
}
