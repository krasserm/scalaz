package scalaz.example

import scalaz._

object ExampleArrowExperimental {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {

    // ----------------------------------------------------------------------------------
    // See also 'Programming with Arrows' http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
    // ----------------------------------------------------------------------------------

    val plus1  = (_: Int) + 1
    val times2 = (_: Int) * 2

    val plus1s  = plus1.arrow[StreamFunction1]
    val times2s = times2.arrow[StreamFunction1]

    def const[A](r: => A) = (_: Any) => r
    
    // Function1 ArrowChoice
    {
      plus1.left[Int] apply Left(1)  assert_≟ Left(2)
      plus1.left      apply Right(1) assert_≟ Right(1)

      plus1.right      apply Left(1) assert_≟ Left(1)
      plus1.right[Int] apply Right(1) assert_≟ Right(2)

      plus1 +++ times2 apply Left(3) assert_≟ Left(4)
      plus1 +++ times2 apply Right(3) assert_≟ Right(6)

      plus1 ||| times2 apply Left(3) assert_≟ 4
      plus1 ||| times2 apply Right(3) assert_≟ 6
    }


    // StreamFunction1 ArrowChoice
    {
      plus1s.left  apply Stream(Left(1), Right(2), Left(3)) assert_≟ Stream(Left(2), Right(2), Left(4))
      plus1s.right apply Stream(Left(1), Right(2), Left(3)) assert_≟ Stream(Left(1), Right(3), Left(3))

      plus1s +++ times2s apply Stream(Left(1), Right(2), Left(3)) assert_≟ Stream(Left(2), Right(4), Left(4))
      plus1s ||| times2s apply Stream(Left(1), Right(2), Left(3)) assert_≟ Stream(2, 4, 4)
    }

    // StreamFunction1 Arrow
    {
      plus1s.first  apply Stream((1,2), (3,4), (5,6)) assert_≟ Stream((2,2), (4,4), (6,6))
      plus1s.second apply Stream((1,2), (3,4), (5,6)) assert_≟ Stream((1,3), (3,5), (5,7))

      plus1s *** times2s apply Stream((1,2), (3,4), (5,6)) assert_≟ Stream((2,4), (4,8), (6,12))
      plus1s &&& times2s apply Stream(1, 2, 3)             assert_≟ Stream((2,2), (3,4), (4,6))
    }

    // ----
    // mapA
    // ----
    //
    // Haskell:
    //
    //  listcase []     = Left ()
    //  listcase (x:xs) = Right(x,xs)
    //
    //  mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
    //  mapA f = arr listcase >>> arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
    //
    //
    {
      def listcase[A](l: List[A]): Either[Unit, (A, List[A])] = l match {
        case x::xs => Right((x, xs))
        case Nil   => Left(())
      }

      def mapA[A](f: (A => A)): (List[A] => List[A]) =
        (listcase[A] _) >>> (const[List[A]](Nil) ||| (f *** mapA(f) >>> ((p: (A, List[A])) => p._1::p._2)))


      mapA((_:Int) + 1) apply List(1,2,3,4,5) assert_≟ List(2,3,4,5,6)
    }

    //
    // Synchronous circuits
    //
    {
      val T = true
      val F = false

      // NOR gate (trivial)
      val nor = ((p: (Boolean, Boolean)) => !(p._1 || p._2)).arrow[StreamFunction1]

      val in1 = Stream(F, F, F, F, T, T, T, T, F, F, F, F)
      val in2 = Stream(T, F, T, F, T, F, T, F, T, F, T, F)
      // out    Stream(F, T, F, T, F, F, F, F, F, T, F, T)

      nor apply in1.zip(in2) assert_≟ Stream(F, T, F, T, F, F, F, F, F, T, F, T)

      // --------------------
      // Rising edge detector
      // --------------------
      //
      // Haskell:
      //
      // edge :: SF Bool Bool -- SF is a stream function arrow
      // edge = arr id &&& delay False >>> arr detect
      //   where detect (a,b) = a && not b
      //
      // delay x = SF (init . (x:))

      // arrows ...
      val id = (identity[Boolean] _).arrow[StreamFunction1]
      val detect = ((p: (Boolean, Boolean)) => p._1 && (!p._2)).arrow[StreamFunction1]
      def delay[A](x: A) = (as: Stream[A]) => Stream.cons(x, as).init

      // rising edge detector
      val edge = (id &&& delay(F)) >>> detect

      // test ...
      val in = Stream(F, F, F, F, T, T, T, T, T, F, F, F, F)
      // out:  Stream(F, F, F, F, T, F, F, F, F, F, F, F, F)

      edge apply Stream(F, F, F, F, T, T, T, T, T, F, F, F, F) assert_≟ Stream(F, F, F, F, T, F, F, F, F, F, F, F, F)
    }
  }
}
