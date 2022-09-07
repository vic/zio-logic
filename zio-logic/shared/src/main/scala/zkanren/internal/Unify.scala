package zkanren.internal

trait Unify[-A, -B] {
  def apply[R, E](a: => A, b: => B): Goal[R, E]
}

object Unify {

  def terms[A]: Unify[LTerm[A], LTerm[A]] = new Unify[LTerm[A], LTerm[A]] {
    override def apply[R, E](a: => LTerm[A], b: => LTerm[A]): Goal[R, E] = Goal.unifyTerm[A](a, b)
  }

  def iterables[A, B](implicit u: Unify[A, B]): Unify[IterableOnce[A], IterableOnce[B]] =
    new Unify[IterableOnce[A], IterableOnce[B]] {
      override def apply[R, E](a: => IterableOnce[A], b: => IterableOnce[B]): Goal[R, E] = { state =>
        val ab = a.iterator.map(Some(_)).zipAll(b.iterator.map(Some(_)), None, None)

        val goals: Iterator[Goal[R, E]] = ab.map[Goal[R, E]] {
          case (Some(a), Some(b)) => u.apply[R, E](a, b)
          case _                  => Goal.left[R, E]
        }

        Goal.sequence[R, E](onRight = true)(goals)(state)
      }
    }

}
