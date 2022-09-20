package zkanren.internal

import zio.Tag
import zio.stm.ZSTM
import zio.stream.ZChannel

// A unifier tries to unify A and B under some goal.
trait Unify[-R, +E, -A, -B] extends ((A, B) => Goal[R, E])

object Unify {
  type Unify1[-R, +E, -A]     = Unify[R, E, A, A]
  type UnifyT[-R, +E, -A, -B] = Unify[R, E, LTerm[A], LTerm[B]]
  type Unify1T[-R, +E, -A]    = UnifyT[R, E, A, A]

  @inline def one[A]: PartiallyApplied[A, A]    = new PartiallyApplied[A, A]
  @inline def two[A, B]: PartiallyApplied[A, B] = new PartiallyApplied[A, B]

  private[Unify] class PartiallyApplied[A, B] private[Unify] () {
    @inline def apply[R, E](f: (A, B) => Goal[R, E]): Unify[R, E, A, B] = f(_, _)
  }

  def never[A]: Unify[Any, Nothing, A, A] = { (_, _) => Goal.reject }

  def identity[A]: Unify[Any, Nothing, A, A] = {
    case (a, b) if a == b => Goal.accept
    case _                => Goal.reject
  }

  def terms[R, E, A: Tag, B: Tag](
    u: Unify[R, E, LTerm[A], LTerm[B]]
  ): Unify[R, E, LTerm[A], LTerm[B]] = { case (a, b) =>
    Goal.fromReadLoop[R, E] { state =>
      val makeChan: ZSTM[R, E, Goal.Chan[R, E]] =
        state.unify(a, b).either.map {
          case Right(_) =>
            ZChannel.write(Right(state))

          case Left(State.UnequalTerms(x: LTerm[A], y: LTerm[B])) =>
            ZChannel.write(state) >>> u(x, y).toChannel

          case _ =>
            ZChannel.write(Left(state))
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
