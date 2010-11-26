package scalaz

trait Arrow[A[_, _]] {
  val category: Category[A]
  
  def arrow[B, C](f: B => C): A[B, C]

  def first[B, C, D](a: => A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: => A[B, C]): A[(D, B), (D, C)]
}

trait ArrowChoice[A[_, _]] extends Arrow[A] {
  def left[B, C, D](a: => A[B, C]): A[Either[B, D], Either[C, D]]

  def right[B, C, D](a: => A[B, C]): A[Either[D, B], Either[D, C]]

  def join[C](e: Either[C, C]) = e match {
    case Left(a) => a
    case Right(b) => b
  }
}

//
// TODO: remaining type classes that extend Arrow
//
// - ArrowLoop
// - ArrowApply
// - ArrowMonad
// - ...
//

object Arrow {
  import Scalaz._

  implicit def Function1Arrow: Arrow[Function1] = new Function1Arrow

  implicit def Function1ArrowChoice: ArrowChoice[Function1] = new Function1ArrowChoice

  implicit def StreamFunction1Arrow: Arrow[StreamFunction1] = new StreamFunction1Arrow

  implicit def StreamFunction1ArrowChoice: ArrowChoice[StreamFunction1] = new StreamFunction1ArrowChoice

  implicit def PartialFunctionArrow: Arrow[PartialFunction] = new Arrow[PartialFunction] {
    val category = Category.PartialFunctionCategory

    def arrow[B, C](f: B => C) = {
      case b => f(b)
    }

    def first[B, C, D](a: => PartialFunction[B, C]) = {
      case (b, d) if a isDefinedAt b => (a(b), d)
    }

    def second[B, C, D](a: => PartialFunction[B, C]): PartialFunction[(D, B), (D, C)] = {
      case (d, b) if a isDefinedAt b => (d, a(b))
    }
  }

  implicit def KleisliArrow[M[_]: Monad]: Arrow[PartialApplyK[Kleisli, M]#Apply] = new Arrow[PartialApplyK[Kleisli, M]#Apply] {
    val category = Category.KleisliCategory

    def arrow[B, C](f: B => C) = ☆(f(_) η)

    def first[B, C, D](a: => Kleisli[M, B, C]) = ☆ {
      case (b, d) => a(b) ∘ ((_, d))
    }

    def second[B, C, D](a: => Kleisli[M, B, C]) = ☆ {
      case (d, b) => a(b) ∘ ((d, _))
    }
  }

  implicit def CokleisliArrow[M[_]: Comonad]: Arrow[PartialApplyK[Cokleisli, M]#Apply] = new Arrow[PartialApplyK[Cokleisli, M]#Apply] {
    val category = Category.CokleisliCategory

    def arrow[B, C](f: B => C) = ★(r => f(r ε))

    def first[B, C, D](a: => Cokleisli[M, B, C]) = ★(a *** arrow(identity(_: D)) apply (_: M[(B, D)]))

    def second[B, C, D](a: => Cokleisli[M, B, C]) = ★(arrow(identity(_: D)) *** a apply (_: M[(D, B)]))
  }

  private[scalaz] class Function1Arrow extends Arrow[Function1] {
    val category = Category.Function1Category

    def arrow[B, C](f: B => C) = f

    def first[B, C, D](a: => (B => C)) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: => (B => C)) =
      (db: (D, B)) => (db._1, a(db._2))
  }

  private[scalaz] class Function1ArrowChoice extends Function1Arrow with ArrowChoice[Function1] {
    def left[B, C, D](a: => (B => C)) = (bd: Either[B, D]) => bd match {
      case Left(b)  => Left(a(b))
      case Right(d) => Right(d)
    }

    def right[B, C, D](a: => (B => C)) = (db: Either[D, B]) => db match {
      case Left(d)  => Left(d)
      case Right(b) => Right(a(b))
    }
  }

  private[scalaz] class StreamFunction1Arrow extends Arrow[StreamFunction1] {
    val category = Category.StreamFunction1Category

    def arrow[B, C](f: B => C) = (s: Stream[B]) => s map f

    def first[B, C, D](a: => StreamFunction1[B,C]) = (sbd: Stream[(B, D)]) => {
      val (s1, s2) = sbd.unzip
      a(s1).zip(s2)
    }

    def second[B, C, D](a: => StreamFunction1[B,C]) = (sdb: Stream[(D, B)]) => {
      val (s1, s2) = sdb.unzip
      s1.zip(a(s2))
    }

  }

  private[scalaz] class StreamFunction1ArrowChoice extends StreamFunction1Arrow with ArrowChoice[StreamFunction1] {
    def left[B, C, D](f: => StreamFunction1[B, C]) =
      (xs: Stream[Either[B, D]]) => combineLeft(xs, f(for (Left(e) <- xs) yield e))

    def right[B, C, D](f: => StreamFunction1[B, C]) =
      (xs: Stream[Either[D, B]]) => combineRight(xs, f(for (Right(e) <- xs) yield e))

    private def combineLeft[B, C, D](s1: Stream[Either[B, D]], s2: => Stream[C]): Stream[Either[C, D]] = (s1, s2) match {
      case (Stream.cons(Left(y),  ys), Stream.cons(z, zs)) => Stream.cons(Left(z),  combineLeft(ys, zs))
      case (Stream.cons(Right(y), ys), zs)                 => Stream.cons(Right(y), combineLeft(ys, zs))
      case (Stream(), zs)                                  => Stream()
    }

    private def combineRight[B, C, D](s1: Stream[Either[D, B]], s2: => Stream[C]): Stream[Either[D, C]] = (s1, s2) match {
      case (Stream.cons(Right(y), ys), Stream.cons(z, zs)) => Stream.cons(Right(z), combineRight(ys, zs))
      case (Stream.cons(Left(y),  ys), zs)                 => Stream.cons(Left(y),  combineRight(ys, zs))
      case (Stream(), zs)                                  => Stream()
    }
  }

  //
  // TODO: remaining type class instances
  //
  // ...
  //
}
