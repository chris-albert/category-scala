package io.lbert.free

import cats.data.{Kleisli, Reader}
import cats.free.Free
import cats.free.Free.liftF
import cats.{Applicative, Id, Monad, ~>}
import language.higherKinds

object Extracts {

  trait GenExtractor[K,V,A] {
    def key: K
    def extract(a: A): V
  }

//  case class User(name: String, age: Int)
//  case class House(number: Double)
//
//  trait Extractable[A]
//  case class ExMap[A,B](m: Map[A,B]) extends Extractable[B]
//  case class ExUser[A](u: User) extends Extractable[A]
//  case class ExHouse[A](h: House) extends Extractable[A]

//  object ExtractableInstances {
//    implicit val userExtractable = new Extractable[User] {}
//    implicit val houseExtractable = new Extractable[House] {}
//  }

  type Key = String

  trait Named {
    def name: Key
  }

  trait NamedExtractorK[M[_], A, B] extends Named {
    def name: Key
  }

  case class NamedExtractor[A,B: Valueable](name: Key, f: A => B)
    extends NamedExtractorK[Id, A, B]

  case class NamedExtractorOpt[A,B: Valueable](name: Key, f: A => Option[B])
    extends NamedExtractorK[Option, A, B]


  sealed trait ExtractorA[A]

  case class Extract[A](k: Key, valuable: Valueable[A]) extends ExtractorA[A]
  case class ExtractOpt[A](k: Key, valuable: Valueable[A]) extends ExtractorA[Option[A]]

  type Extractor[A] = Free[ExtractorA, A]

  def extract[A: Valueable](k: Key): Extractor[A] =
    liftF[ExtractorA,A](Extract[A](k,implicitly[Valueable[A]]))

  def extract[A: Valueable](named: Named): Extractor[A] =
    extract[A](named.name)

  def extractOpt[A: Valueable](k: Key): Extractor[Option[A]] =
    liftF[ExtractorA,Option[A]](ExtractOpt[A](k, implicitly[Valueable[A]]))

  def extractOpt[A: Valueable](named: Named): Extractor[Option[A]] =
    extractOpt[A](named.name)


  def compute(m: Map[Key, Value]): ExtractorA ~> Option =
    new (ExtractorA ~> Option) {
      override def apply[A](fa: ExtractorA[A]): Option[A] =
        fa match {
          case Extract(k,v) =>
            m.get(k).flatMap(value => v.fromValue(value))
          case ExtractOpt(k,v) =>
            m.get(k).flatMap(value => v.fromValue(value)) match {
              case Some(d) => Some(Some(d))
              case _ => Some(None)
            }
        }
    }

  type ExtractorReader[A] = Reader[Map[Key, Value], Option[A]]

  val computeReader: ExtractorA ~> ExtractorReader =
    new (ExtractorA ~> ExtractorReader) {
      override def apply[A](fa: ExtractorA[A]): ExtractorReader[A] =
        fa match {
          case Extract(k,v) =>
            Reader(m => m.get(k).flatMap(value => v.fromValue(value)))
          case ExtractOpt(k,v) =>
            Reader(m =>
              m.get(k).flatMap(value => v.fromValue(value)) match {
                case Some(d) => Some(Some(d))
                case _ => Some(None)
              }
            )
        }
    }

  type ExtractFunction[A] = Function[A,Value]

//  val extractCompiler: ExtractorA ~> Reader =
//    new (ExtractorA ~> Reader) {
//      override def apply[A](fa: ExtractorA[A]): Reader[A] = {
//        fa match {
//          case Extract(k,v) =>
//            ???
//          case ExtractOpt(k,v) =>
//            ???
//          case ExtractA(k,v,f) =>
//
//            ???
//        }
//      }
//    }
}
