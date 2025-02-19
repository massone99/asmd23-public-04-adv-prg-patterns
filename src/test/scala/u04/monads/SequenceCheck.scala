package scala.u04.monads

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import u04.datastructures.*
import Sequences.*
import Sequence.*

// Properties class for testing Sequence implementation using property-based testing
object SequenceCheck extends Properties("Sequence"):

  // define a recursive generator of lists, monadically
  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] = for
    i <- arbitrary[A] // Generate random element of type A
    b <- Gen.prob(0.8) // 80% chance to continue sequence
    // Chiamata ricorsiva per generare la coda della lista, la generazione ricorsiva
    s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s

  // define custom arbitrary lists and mappers
  // intSeqArbitrary: generates random sequences of integers
  given intSeqArbitrary: Arbitrary[Sequence[Int]] = Arbitrary(
    sequenceGen[Int]()
  )
  // mapperArbitrary: generates random functions to transform integers
  // Available functions: increment (+1), double (*2), square (x^2)
  given mapperArbitrary: Arbitrary[Int => Int] = Arbitrary(
    Gen.oneOf[Int => Int](_ + 1, _ * 2, x => x * x)
  )

  // check axioms, universally
  property("mapAxioms") = forAll: (seq: Sequence[Int], f: Int => Int) =>
    // println(seq); println(f(10)) // inspect what's using
    (seq, f) match
      // 1. mapping over an empty list (Nil) should return Nil
      case (Nil(), f) => map(Nil())(f) == Nil()
      // 2. mapping over Cons(h,t) should equal Cons(f(h), map(t)(f))
      case (Cons(h, t), f) => map(Cons(h, t))(f) == Cons(f(h), map(t)(f))

  // how to check a generator works as expected
  @main def showSequences() =
    Range(0, 20).foreach(i =>
      println(summon[Arbitrary[Sequence[Int]]].arbitrary.sample)
    )
