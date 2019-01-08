package io.lbert

object Poop {


  trait Key
  trait Value

  trait Keyable[A] {
    def from(k: Key): A
    def to(a: A): Key
  }

  trait Valueable[A] {
    def from(v: Value): A
    def to(a: A): Value
  }

<<<<<<< HEAD
//  type Extractor[A, B: Keyable,C: Valueable] = A => Map[B,C]

=======
//  type Extractor[A,B: Keyable[A],C: Valueable[A]] = A => Map[B,C]
//
>>>>>>> 3084d1ddb002f7c55e62e17bb9c8c403a16b2012
//  type Mapper[B: Keyable,C: Valueable,D: Valueable] = Map[B,C] => Map[B,D]


}
