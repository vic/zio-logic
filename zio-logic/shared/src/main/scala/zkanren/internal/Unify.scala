package zkanren.internal

trait Unify[-R, +E, -A, -B] extends ((A, B) => Goal[R, E])

object Unify {

  def one[A]: PartiallyApplied[A, A]      = new PartiallyApplied[A, A]
  def apply[A, B]: PartiallyApplied[A, B] = new PartiallyApplied[A, B]

  private[Unify] class PartiallyApplied[A, B]() {
    def apply[R, E](f: (A, B) => Goal[R, E]): Unify[R, E, A, B] = (a, b) => f(a, b)
  }

  private[zkanren] def terms[A]: Unify[Any, Nothing, LTerm[A], LTerm[A]] = Goal.unifyTerm[A]

  private[zkanren] def iterables[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, IterableOnce[A], IterableOnce[B]] = { case (a, b) =>
    val ab = a.iterator.map(Some(_)).zipAll(b.iterator.map(Some(_)), None, None)

    val goals: Iterator[Goal[R, E]] = ab.map {
      case (Some(a), Some(b)) => u(a, b)
      case _                  => Goal.left
    }

    Goal.sequence()(goals)
  }

}
