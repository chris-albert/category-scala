package io.lbert.shapeless

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def writeCsv[A: CsvEncoder](values: List[A]): String =
    values.map(value => implicitly[CsvEncoder[A]].encode(value).mkString(",")).mkString("\n")

  implicit def pairEncoder[A: CsvEncoder,B: CsvEncoder]: CsvEncoder[(A,B)] = new CsvEncoder[(A, B)] {
    override def encode(value: (A, B)): List[String] = {
      val (a,b) = value
      implicitly[CsvEncoder[A]].encode(a) ++ implicitly[CsvEncoder[B]].encode(b)
    }
  }

  // Summoner method
  def apply[A: CsvEncoder]: CsvEncoder[A] = implicitly[CsvEncoder[A]]

  // Constructor method
  def instance[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    override def encode(value: A): List[String] = func(value)
  }

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    instance(b => if(b) List("yes") else List("no"))

  implicit val stringEncoder: CsvEncoder[String] =
    instance(s => List(s))

  implicit val intEncoder: CsvEncoder[Int] =
    instance(i => List(i.toString))

  implicit val doubleEncoder: CsvEncoder[Double] =
    instance(d => List(d.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    instance(_ => Nil)

  implicit val cnilEncoder: CsvEncoder[CNil] =
    instance(_ => throw new Exception("Wat"))

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    headEncoder: Lazy[CsvEncoder[H]],
    tailEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = instance {
    case Inl(h) => headEncoder.value.encode(h)
    case Inr(t) => tailEncoder.encode(t)
  }

  implicit def hlistEncoder[H, T <: HList](
    implicit
    headEncoder: Lazy[CsvEncoder[H]],
    tailEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    CsvEncoder.instance[H :: T] {
      case h :: t => headEncoder.value.encode(h) ++ tailEncoder.encode(t)
    }

  implicit def genericEncoder[A,R](
    implicit
    gen: Generic.Aux[A,R],
    enc: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = instance(a => enc.value.encode(gen.to(a)))
}

case class Employee(name: String, number: Int, manager: Boolean)

object Employee {

  implicit val employeeEncoder: CsvEncoder[Employee] = new CsvEncoder[Employee] {
    override def encode(value: Employee): List[String] = List(
      value.name,
      value.number.toString,
      if(value.manager) "yes" else "no"
    )
  }


}
