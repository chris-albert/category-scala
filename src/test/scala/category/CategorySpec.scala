package category

import org.scalatest.{Matchers, WordSpec}
import Category._

class CategorySpec extends WordSpec with Matchers {

  "A functor" must {
    import Implicits.FunctorImplicits._
    "pass the identity functor law" in {
      val box1 = Box(1)
      Functor[Box].map(box1)(identity) shouldBe box1
    }
    "pass the composition functor law" in {
      val box1 = Box(1)
      val f = (i: Int) => i + 1
      val g = (i: Int) => i + 3
      Functor[Box].map(Functor[Box].map(box1)(f))(g) shouldBe Functor[Box].map(box1)(f andThen g)
    }
    "work with implicit option and apply" in {
      Functor[Option].map(Some(1))(_ + 2) shouldBe Some(3)
      Functor.apply[Option].map(Some(1))(_ + 2) shouldBe Some(3)

      val f: Int => Int = (i: Int) => i + 1
    }
  }

  "A applicative" must {
    import Implicits.ApplicativeImplicits._
    import Implicits.FunctorImplicits.FunctorInfix
    "add 2 boxes together" in {
      val box1 = Box(1)
      val box2 = Box(2)
      val f: Int => Int => Int = (i1: Int) => (i2: Int) => i1 + i2
      val fo: (Int,Int) => Int = (i1: Int,i2: Int) => i1 + i2
      val boxF: Box[Int => Int => Int] = Applicative[Box].pure(f)

      val firstApply: Box[Int => Int] = Applicative[Box].apply(box1)(boxF)
      val secondApply: Box[Int] = Applicative[Box].apply(box2)(firstApply)

      secondApply shouldBe Box(3)

      //or using map from Functor and not wrapping in pure

      val fApply: Box[Int => Int] = Applicative[Box].map(box1)(f)
      val sApply: Box[Int] = Applicative[Box].apply(box2)(fApply)

      sApply shouldBe Box(3)
    }
    "add 2 boxes together, with infix" in {
      val box1 = Box(1)
      val box2 = Box(2)
      val f: Int => Int => Int = (i1: Int) => (i2: Int) => i1 + i2
      val boxA:      Box[Int => Int => Int] = Applicative[Box].pure(f)

      boxA infixApply box1 infixApply box2 shouldBe Box(3)

      //With haskell syntax
      boxA `<*>` box1 `<*>` box2 shouldBe Box(3)

      //With more sugar from the Functor infix
      f `<$>` box1 `<*>` box2 shouldBe Box(3)
    }
  }

  "A ReaderMonad" must {
    "be able to apply directly to reader" in {
      val triple: ReaderMonad[Int,Int] = ReaderMonad((i: Int) => i * 3)

      triple(3) shouldBe 9
      triple(2) shouldBe 6
      triple(10) shouldBe 30
    }
    "be able to map onto a reader" in {
      val triple: ReaderMonad[Int,Int] = ReaderMonad((i: Int) => i * 3)

      val triplePlus2: ReaderMonad[Int,Int] = triple.map(i => i + 2)
      triplePlus2(3) shouldBe 11

      val triplePlus2ToString: ReaderMonad[Int,String] = triplePlus2.map(_.toString)
      triplePlus2ToString(4) shouldBe "14"
    }
    "be able to for comprehension on a reader" in {
      val triple: ReaderMonad[Int,Int] = ReaderMonad((i: Int) => i * 3)

      val plusTwo: ReaderMonad[Int,Int] = ReaderMonad((i: Int) => i + 2)

      val toString: ReaderMonad[Int,String] = ReaderMonad((i: Int) => i.toString)

      val resultReader: ReaderMonad[Int,(Int,Int,String)] = for {
        t <- triple
        p <- plusTwo
        s <- toString
      } yield (t,p,s)

      resultReader(3) shouldBe (9,5,"3")
    }
  }
}
