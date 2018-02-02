package cats

import org.scalatest.{Matchers, WordSpec}
import cats.instances.option._
import CatsFree._
import cats.CatsFree.ValuableInstances._

class CatsFreeSpec extends WordSpec with Matchers {

  "Extracts" should {
    def program =
      for {
        a <- extract[Double]("a")
        b <- extract[Double]("b")
        c <- extractOpt[Double]("c")
      } yield a + b + c.getOrElse(0d)

    "get none if required dependecy doesn't exist" in {
      program.foldMap(compute(Map())) shouldBe None
    }
    "get value if all dependencies are met" in {
      program.foldMap(compute(Map(
        "a" -> DoubleVal(10),
        "b" -> DoubleVal(100)
      ))) shouldBe Some(110)
    }
    "get value if all dependencies are met, including optionals" in {
      program.foldMap(compute(Map(
        "a" -> DoubleVal(10),
        "b" -> DoubleVal(100),
        "c" -> DoubleVal(1)
      ))) shouldBe Some(111)
    }
  }

  "Extracts from A" should {
    case class User(name: String, age: Int)
    case class House(number: Double)
    def program =
      for {
        a <- extractA[User,Int]("age",_.age)
        b <- extract[Double]("b")
        h <- extractA[House, Double]("houseNum",_.number)
      } yield a + b + h

    "get value" in {
      program.foldMap(compute(Map(
        "age" -> IntVal(10),
        "b" -> DoubleVal(11),
        "houseNum" -> DoubleVal(100)
      ))) shouldBe Some(121d)
    }

    "extract values" in {
//      val a = program.foldMap(extractCompiler)
    }
  }
}
