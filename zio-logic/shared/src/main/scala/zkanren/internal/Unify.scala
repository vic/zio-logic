package zkanren.internal

import zio.stm.ZSTM
import zio.stream.ZChannel

// A unifier tries to unify A and B under some goal.
trait Unify[-R, +E, -A, -B] extends ((A, B) => Goal[R, E])

object Unify {
  @inline def one[A]: PartiallyApplied[A, A]      = new PartiallyApplied[A, A]
  @inline def apply[A, B]: PartiallyApplied[A, B] = new PartiallyApplied[A, B]

  private[Unify] class PartiallyApplied[A, B] private[Unify] () {
    @inline def apply[R, E](f: (A, B) => Goal[R, E]): Unify[R, E, A, B] = f(_, _)
  }

  def identity[A]: Unify[Any, Nothing, A, A] = {
    case (a, b) if a == b => Goal.accept
    case _                => Goal.reject
  }

  def terms[R, E, A](implicit u: Unify[R, E, A, A]): Unify[R, E, LTerm[A], LTerm[A]] = { case (a, b) =>
    Goal.fromReadLoop[R, E] { state =>
      val makeChan: ZSTM[R, E, Goal.Chan[R, E]] =
        state.unify(a, b).either.map {
          case Right(_)           => ZChannel.write(Right(state))
          case Left(None)         => ZChannel.write(Left(state))
          case Left(Some((a, b))) => ZChannel.write(state) >>> u(a.value, b.value).toChannel
        }
      ZChannel.unwrap(makeChan.commit)
    }
  }

  def iterables[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, IterableOnce[A], IterableOnce[B]] = { case (a, b) =>
    val ab = a.iterator.map(Some(_)).zipAll(b.iterator.map(Some(_)), None, None)

    val goals: Iterator[Goal[R, E]] = ab.map {
      case (Some(a), Some(b)) => u(a, b)
      case _                  => Goal.reject
    }

    Goal.conj(goals)
  }

  def headOfIterable[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, A, Iterable[B]] = { case (a, bs) =>
    iterables(u)(Some(a), bs.headOption)
  }

  def tailOfIterable[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, IterableOnce[A], Iterable[B]] = {
    case (as, bs) => iterables(u)(as, bs.tail)
  }

}
