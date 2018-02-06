package io.lbert.free

import cats.free.Free
import cats.free.Free.liftF
import cats.~>

object Extracts {

  trait GenExtractor[K,V,A] {
    def key: K
    def extract(a: A): V
  }

  case class User(name: String, age: Int)
  case class House(number: Double)

  trait Extractable[A]
//  case class ExMap[A,B](m: Map[A,B]) extends Extractable[B]
//  case class ExUser[A](u: User) extends Extractable[A]
//  case class ExHouse[A](h: House) extends Extractable[A]

  object ExtractableInstances {
    implicit val userExtractable = new Extractable[User] {}
    implicit val houseExtractable = new Extractable[House] {}
  }

  sealed trait ExtractorA[A]
  type Key = String

  case class Extract[A](k: Key, valuable: Valueable[A]) extends ExtractorA[A]
  case class ExtractOpt[A](k: Key, valuable: Valueable[A]) extends ExtractorA[Option[A]]
  case class ExtractA[A,B](k: Key, valuable: Valueable[B], f: A => B) extends ExtractorA[B]

  type Extractor[A] = Free[ExtractorA, A]

  def extract[A: Valueable](k: Key): Extractor[A] =
    liftF[ExtractorA,A](Extract[A](k,implicitly[Valueable[A]]))

  def extractOpt[A: Valueable](k: Key): Extractor[Option[A]] =
    liftF[ExtractorA,Option[A]](ExtractOpt[A](k, implicitly[Valueable[A]]))

  def extractA[A, B: Valueable](k: Key,f: A => B): Extractor[B] =
    liftF[ExtractorA,B](ExtractA[A,B](k, implicitly[Valueable[B]], f))


  def compute(m: Map[Key, Value]): ExtractorA ~> Option =
    new (ExtractorA ~> Option) {
      override def apply[A](fa: ExtractorA[A]): Option[A] =
        fa match {
          case Extract(k,v) =>
            m.get(k).flatMap(value => v.fromValue(value))
          case ExtractA(k,v,_) =>
            m.get(k).flatMap(value => v.fromValue(value))
          case ExtractOpt(k,v) =>
            val a = m.get(k).flatMap(value => v.fromValue(value))
            a match {
              case Some(d) => Some(Some(d))
              case _ => Some(None)
            }
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
