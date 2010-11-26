package scalaz

sealed trait MAB[M[_, _], A, B] extends PimpedType[M[A, B]] {
  def asMAB: MAB[M, A, B] = this

  def :->[D](g: B => D)(implicit b: Bifunctor[M]): M[A, D] = b.bimap(value, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]): M[C, B] = b.bimap(value, f, identity[B])

  def >>>[C](k: M[B, C])(implicit c: Category[M]): M[A, C] = c compose (k, value)
 
  def ⋙[C](k: M[B, C])(implicit c: Category[M]): M[A, C] = c compose (k, value)

  def <<<[C](k: M[C, A])(implicit c: Category[M]): M[C, B] = c compose (value, k)
 
  def ⋘[C](k: M[C, A])(implicit c: Category[M]): M[C, B] = c compose (value, k)

  def first[C](implicit a: Arrow[M]): M[(A, C), (B, C)] = a first value

  def second[C](implicit a: Arrow[M]): M[(C, A), (C, B)] = a second value

  def left[C](implicit a: ArrowChoice[M]): M[Either[A, C], Either[B, C]] = a left value

  def right[C](implicit a: ArrowChoice[M]): M[Either[C, A], Either[C, B]] = a right value

  def ***[C, D](k: => M[C, D])(implicit a: Arrow[M]): M[(A, C), (B, D)] = a.category.compose(a.second[C, D, B](k), first[C])

  def &&&[C](k: => M[A, C])(implicit a: Arrow[M]): M[A, (B, C)] = a.category.compose(***(k), a.arrow(a => (a, a)))

  def +++[C, D](k: => M[C, D])(implicit a: ArrowChoice[M]): M[Either[A, C], Either[B, D]] = a.category.compose(a.right[C, D, B](k), left[C])

  def |||[C](k: => M[C, B])(implicit a: ArrowChoice[M]): M[Either[A, C], B] = a.category.compose(a.arrow(a.join[B]), +++(k))

  def product(implicit a: Arrow[M]): M[(A, A), (B, B)] = this *** value

  def ^>>[C](f: C => A)(implicit a: Arrow[M]): M[C, B] = a.category.compose(value, a.arrow(f))

  def >>^[C](f: B => C)(implicit a: Arrow[M]): M[A, C] = a.category.compose(a.arrow(f), value)

  def <<^[C](f: C => A)(implicit a: Arrow[M]): M[C, B] = a.category.compose(value, a.arrow(f))

  def ^<<[C](f: B => C)(implicit a: Arrow[M]): M[A, C] = a.category.compose(a.arrow(f), value)
}

trait MABs {
  implicit def mab[M[_, _], A, B](a: M[A, B]): MAB[M, A, B] = new MAB[M, A, B] {
    val value = a
  }
}
