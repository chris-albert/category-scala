package io.lbert.shapeless

//import io.lbert.shapeless.CsvEncoder._

sealed trait Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {
  val treeEncoder = CsvEncoder[Tree[Int]]
}
