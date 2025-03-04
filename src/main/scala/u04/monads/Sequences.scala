package u04.monads
import Monads.*, Monad.*

object Sequences:

  // Un'implementazione che mimica una List[A]
  enum Sequence[A]:
    case Cons(h: A, t: Sequence[A])
    case Nil()

  object Sequence:
    // Extension method for Sequence[A] to append another Sequence[A]
    extension [A](s1: Sequence[A])
      def append(s2: Sequence[A]): Sequence[A] = s1 match
        case Cons(h, t) => Cons(h, t.append(s2))
        case Nil()      => s2

  given Monad[Sequence] with
    import Sequence.*

    /** The unit method which, like in all monads, wraps a value into a monad
      *
      * @param a
      *   the value to wrap
      * @return
      *   the value wrapped into the monad
      */
    def unit[A](a: A) = Cons(a, Nil())

    extension [A](m: Sequence[A])
      /** The flatMap method which, like in all monads, applies a function to
        * each element of the monad while keeping the value wrapped into the monad
        *
        * @param f
        *   the function to apply
        * @return
        *   the monad with the function applied
        */
      def flatMap[B](f: A => Sequence[B]): Sequence[B] =
        m match
          case Cons(h, t) => f(h).append(t.flatMap(f))
          case Nil()      => Nil()

@main def trySequences =
  import Sequences.{*, given}, Sequence.*

  // for comprehension: <- just iterates all element,
  // using each to create a new box
  val s: Sequence[(Int, String)] = for
    x <- Cons(10, Cons(20, Nil()))
    y <- Cons("a", Cons("b", Nil()))
    z <- Cons(true, Cons(false, Nil()))
  yield if z then (x, y) else (0, y)

  // println(s)

  val tUnit = summon[Monad[Sequence]].unit(10)
  println(tUnit) // Cons(10,Nil())
  // Cons((10,a),Cons((0,a),Cons((10,b),Cons((0,b),
  // Cons((20,a),Cons((0,a),Cons((20,b),Cons((0,b),Nil()))))))))
