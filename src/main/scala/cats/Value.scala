package cats

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
