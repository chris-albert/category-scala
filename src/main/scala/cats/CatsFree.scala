package cats

import cats.free.Free
import cats.free.Free.liftF

object CatsFree {

  sealed trait Value
  case class DoubleVal(d: Double) extends Value
  case class IntVal(i: Int) extends Value
  case class StringVal(s: String) extends Value

  trait Valueable[A] {
    def toValue(a: A): Value
    def fromValue(v: Value): Option[A]
  }

  object Valueable {
    def apply[A](
      tv: A => Value,
      fv: Value => Option[A]
    ): Valueable[A] = new Valueable[A] {
      override def toValue(a: A) = tv(a)
      override def fromValue(v: Value) = fv(v)
    }
  }

  object ValuableInstances {

    implicit val doubleValuable = Valueable[Double](
      DoubleVal, {
        case DoubleVal(d) => Some(d)
        case _ => None
      }
    )

    implicit val intValuable = Valueable[Int](
      IntVal, {
        case IntVal(i) => Some(i)
        case _ => None
      }
    )

    implicit val stringValuable = Valueable[String](
      StringVal, {
        case StringVal(s) => Some(s)
        case _ => None
      }
    )
  }

  sealed trait ExtractorA[A]

  case class Extract[A](k: String, valuable: Valueable[A]) extends ExtractorA[A]
  case class ExtractOpt[A](k: String, valuable: Valueable[A]) extends ExtractorA[Option[A]]

  type Extractor[A] = Free[ExtractorA, A]

  def extract[A: Valueable](k: String): Extractor[A] =
    liftF[ExtractorA,A](Extract[A](k,implicitly[Valueable[A]]))

  def extractOpt[A: Valueable](k: String): Extractor[Option[A]] =
    liftF[ExtractorA,Option[A]](ExtractOpt[A](k, implicitly[Valueable[A]]))


  def compiler(m: Map[String, Value]): ExtractorA ~> Option =
    new (ExtractorA ~> Option) {
      override def apply[A](fa: ExtractorA[A]): Option[A] =
        fa match {
          case Extract(k,v) =>
            m.get(k).flatMap(value => v.fromValue(value))
          case ExtractOpt(k,v) =>
            val a = m.get(k).flatMap(value => v.fromValue(value))
            a match {
              case Some(d) => Some(Some(d))
              case _ => Some(None)
            }
        }
    }

}
