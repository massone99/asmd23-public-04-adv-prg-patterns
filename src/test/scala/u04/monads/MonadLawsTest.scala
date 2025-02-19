package scala.u04.monads

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import u04.datastructures.*
import u04.monads.Monads.Monad
import u04.monads.Sequences.{Sequence, given}

// desugared would be [M[_]](using Monad[M])
abstract class MonadLawsTest[M[_]: Monad] extends Properties("Monad"):
  implicit def arbitraryMonad[A: Arbitrary]: Arbitrary[M[A]]

  private val M = summon[Monad[M]]

  property("leftIdentity") = forAll: (x: Int, f: Int => M[Int]) =>
    M.unit(x).flatMap(f) == f(x)

  property("rightIdentity") = forAll: (m: M[Int]) =>
    m.flatMap(M.unit) == m

  property("associativity") = forAll: (m: M[Int], f: Int => M[Int], g: Int => M[Int]) =>
    m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))