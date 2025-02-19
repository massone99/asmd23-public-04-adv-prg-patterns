package scala.u04.monads

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import u04.datastructures.*
import u04.monads.Monads.Monad
import u04.monads.Sequences.{Sequence, given}

object SequenceMonadAxiomsCheck extends MonadLawsTest[Sequence]:
  import Sequence.*
  

  override implicit def arbitraryMonad[A: Arbitrary]: Arbitrary[Sequence[A]] = Arbitrary(sequenceGen[A]())


  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] =
    for
      i <- arbitrary[A]
      b <- Gen.prob(0.8)
      s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
    yield s

  given Arbitrary[Sequence[Int]] = Arbitrary(sequenceGen[Int]())

  