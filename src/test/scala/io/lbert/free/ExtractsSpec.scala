package io.lbert.free

import java.util.Date
import org.scalatest.{Matchers, WordSpec}
import cats.instances.option._
import cats.data.Kleisli
import Extracts._
import cats.Id
import io.lbert.free.ValuableInstances._

class ExtractsSpec extends WordSpec with Matchers {

  "Extracts" should {
    def program =
      for {
        a <- extract[Double]("a")
        b <- extract[Double]("b")
        c <- extractOpt[Double]("c")
      } yield a + b + c.getOrElse(0d)

    "get none if required dependency doesn't exist" in {
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
    case class User(name: String, age: Int, middle: Option[String] = None)
    case class House(number: Double)

    val ageExtractor = NamedExtractor[User, Int]("age",_.age)
    val numberExtractor = NamedExtractor[House, Double]("houseNum",_.number)
    val middleExtractor = NamedExtractorOpt[User, String]("middle",_.middle)

    def program =
      for {
        a <- extract[Int](ageExtractor)
        b <- extract[Double]("b")
        h <- extract[Double](numberExtractor)
        d <- extractOpt[String](middleExtractor)
      } yield a + b + h + d.map(_.length).getOrElse(1)

    "get value" in {
      program.foldMap(compute(Map(
        "age" -> IntVal(10),
        "b" -> DoubleVal(11),
        "houseNum" -> DoubleVal(100)
      ))) shouldBe Some(122d)
    }

    "extract values" in {
//      val a = program.foldMap(extractCompiler)
    }
  }
}
