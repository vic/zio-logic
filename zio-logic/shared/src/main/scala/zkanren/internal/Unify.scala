package zkanren.internal

trait Unify[-A, -B] {
  def apply[R, E](a: A, b: B): Goal[R, E]
}

object Unify {

  def same[A]: PartiallyApplied[A, A]     = new PartiallyApplied[A, A]
  def apply[A, B]: PartiallyApplied[A, B] = new PartiallyApplied[A, B]

  private[Unify] class PartiallyApplied[A, B]() {
    def apply(f: (A, B) => Goal[Any, Nothing]): Unify[A, B] = new Unify[A, B] {
      override def apply[R, E](a: A, b: B): Goal[R, E] = f(a, b)
    }
  }

  private[zkanren] def terms[A]: Unify[LTerm[A], LTerm[A]] = Unify.same[LTerm[A]] { case (a, b) =>
    Goal.unifyTerm[A](a, b)
  }

  private[zkanren] def iterables[A, B](implicit u: Unify[A, B]): Unify[IterableOnce[A], IterableOnce[B]] =
    Unify[IterableOnce[A], IterableOnce[B]] { case (a, b) =>
      val ab = a.iterator.map(Some(_)).zipAll(b.iterator.map(Some(_)), None, None)

      val goals: Iterator[Goal[Any, Nothing]] = ab.map {
        case (Some(a), Some(b)) => u(a, b)
        case _                  => Goal.left
      }

      Goal.sequence()(goals)
    }

}
