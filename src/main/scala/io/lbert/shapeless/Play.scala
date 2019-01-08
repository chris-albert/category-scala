package io.lbert.shapeless

import shapeless.{HList, ::, HNil, Coproduct, :+:, CNil, Inl, Inr}

object Play {

  sealed trait Shape

  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val rect: Shape = Rectangle(3.0,4.0)
  val circ: Shape = Circle(1.0)

  def area(shape: Shape): Double = shape match {
    case Rectangle(w,h) => w * h
    case Circle(r) => math.Pi * r * r
  }

  val product: String :: Int :: Boolean :: HNil =
    "Sunday" :: 1 :: false :: HNil

  val first: String = product.head
  val second: Int = product.tail.head
  val rest: Boolean :: HNil = product.tail.tail

  val newProduct: Long :: String :: Int :: Boolean :: HNil =
    42L :: product

  //2.2.1
  import shapeless.Generic

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreamGen = Generic[IceCream]

  val iceCream: IceCream = IceCream("Sundae", 1, false)

  val repr: String :: Int :: Boolean :: HNil = iceCreamGen.to(iceCream)

  val iceCream2: IceCream = iceCreamGen.from(repr)

  case class Employee(name: String, number: Int, manager: Boolean)

  //Create an employee from an ice cream
  val employee: Employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

  //2.3 Generic Coproducts

  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  val red: Light = Inl(Red())
  val green: Light = Inr(Inr(Inl(Green())))

  //2.3.1 Switching encodings using Generic

  type Shapes = Rectangle :+: Circle :+: CNil

  val genericShape = Generic[Shape]

  val coproductShape = genericShape.to(Rectangle(3.0,4.0))
}
