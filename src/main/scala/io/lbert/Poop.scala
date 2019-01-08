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

//  type Extractor[A, B: Keyable,C: Valueable] = A => Map[B,C]

//  type Mapper[B: Keyable,C: Valueable,D: Valueable] = Map[B,C] => Map[B,D]


}
