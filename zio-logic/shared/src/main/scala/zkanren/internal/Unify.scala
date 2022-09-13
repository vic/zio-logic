package zkanren.internal

import zio.ZIO

trait Unify[-R, +E, -A, -B] extends ((A, B) => Goal[R, E])

private[internal] object Unify {
  @inline def one[A]: PartiallyApplied[A, A]      = new PartiallyApplied[A, A]
  @inline def apply[A, B]: PartiallyApplied[A, B] = new PartiallyApplied[A, B]

  private[Unify] class PartiallyApplied[A, B] private[Unify] () {
    @inline def apply[R, E](f: (A, B) => Goal[R, E]): Unify[R, E, A, B] = f(_, _)
  }

  private[zkanren] def terms[A]: Unify[Any, Nothing, LTerm[A], LTerm[A]] = { case (a, b) =>
    Goal.fromZIO[Any, Nothing](
      ZIO.serviceWithZIO[State](s => s.unify(a, b).fold(_ => Left(s), _ => Right(s)).commit)
    )
  }

  private[zkanren] def options[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, Option[A], Option[B]] = {
    case (Some(a), Some(b)) => u(a, b)
    case (None, None)       => Goal.accept
    case _                  => Goal.reject
  }

  private[zkanren] def iterables[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, IterableOnce[A], IterableOnce[B]] = { case (a, b) =>
    val ab = a.iterator.map(Some(_)).zipAll(b.iterator.map(Some(_)), None, None)

    val goals: Iterator[Goal[R, E]] = ab.map {
      case (Some(a), Some(b)) => u(a, b)
      case _                  => Goal.reject
    }

    Goal.conj(goals)
  }

  // Unifies if a is the head of bs.
  private[zkanren] def headOfIterable[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, A, Iterable[B]] = { case (a, bs) => options(u)(Some(a), bs.headOption) }

  // Unifies if as is the tail of bs.
  private[zkanren] def tailOfIterable[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, Iterable[A], Iterable[B]] = { case (as, bs) => iterables(u)(as, bs.tail) }

}
