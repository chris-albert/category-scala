package cats

import org.scalatest.{Matchers, WordSpec}
import cats.instances.option._
import CatsFree._
import cats.CatsFree.ValuableInstances._

class CatsFreeSpec extends WordSpec with Matchers {

  "program" should {

    def program =
      for {
        a <- extract[Double]("a")
        b <- extract[Double]("b")
        c <- extractOpt[Double]("c")
      } yield a + b + c.getOrElse(0d)

    "get none if required dependecy doesn't exist" in {
      program.foldMap(compiler(Map())) shouldBe None
    }
    "get value if all dependencies are met" in {
      program.foldMap(compiler(Map(
        "a" -> DoubleVal(10),
        "b" -> DoubleVal(100)
      ))) shouldBe Some(110)
    }
    "get value if all dependencies are met, including optionals" in {
      program.foldMap(compiler(Map(
        "a" -> DoubleVal(10),
        "b" -> DoubleVal(100),
        "c" -> DoubleVal(1)
      ))) shouldBe Some(111)
    }
  }
}
