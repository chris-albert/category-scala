package io.lbert.shapeless

import io.lbert.shapeless.Play.{Circle, IceCream, Rectangle, Shape}
import org.scalatest.{Matchers, WordSpec}
import shapeless.{::, Generic, HList, HNil}

class CsvEncoderSpec extends WordSpec with Matchers {

  "writeCsv" should {
    "write correct csv" in {
      val employees = List(
        Employee("Bill", 1, true),
        Employee("Peter", 2, false),
        Employee("Milton", 3, false)
      )

      CsvEncoder.writeCsv(employees) shouldBe
        """Bill,1,yes
          |Peter,2,no
          |Milton,3,no""".stripMargin
    }
    "use the summoner" in {
      val summoned = CsvEncoder[Employee]
    }
    "get encoder for an HList" in {
      val repr: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

      val res = repr.encode("abc" :: 123 :: true :: HNil)
      res shouldBe List("abc", "123", "yes")
    }
    "gen encoder for case class" in {
      val gen = Generic[IceCream]
      val enc = CsvEncoder[gen.Repr]
      implicit val iceCreamEncoder = CsvEncoder.instance[IceCream](ic => enc.encode(gen.to(ic)))
      CsvEncoder.writeCsv(List(
        IceCream("Sundae", 1, false),
        IceCream("Cornetto", 0, true),
        IceCream("Banana Split", 0, false)
      )) shouldBe
        """Sundae,1,no
          |Cornetto,0,yes
          |Banana Split,0,no""".stripMargin
    }
    "encode arbitrary case class" in {
      CsvEncoder.writeCsv(List(
        Employee("Sam",1,true),
        Employee("Chris",2,false)
      )) shouldBe
        """Sam,1,yes
          |Chris,2,no""".stripMargin
    }
    "encode coproducts" in {
      val shapes: List[Shape] = List(
        Rectangle(3.0, 4.0),
        Circle(1.0)
      )
      CsvEncoder.writeCsv(shapes) shouldBe
        """3.0,4.0
          |1.0""".stripMargin
    }
  }
}
