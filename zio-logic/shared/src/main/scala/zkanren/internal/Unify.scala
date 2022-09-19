package zkanren.internal

import izumi.reflect.macrortti.LightTypeTag
import zio.{Tag, ZIO}
import zio.stm.ZSTM
import zio.stream.ZChannel
import zkanren.internal.Goal.Chan

// A unifier tries to unify A and B under some goal.
trait Unify[-R, +E, -A, -B] extends ((A, B) => Goal[R, E])

object Unify {
  type UMap = Map[LightTypeTag, Any]

  type U[-A] = Unify[Any, Nothing, A, A]

  @inline def one[A]: PartiallyApplied[A, A]      = new PartiallyApplied[A, A]
  @inline def apply[A, B]: PartiallyApplied[A, B] = new PartiallyApplied[A, B]

  private[Unify] class PartiallyApplied[A, B] private[Unify] () {
    @inline def apply[R, E](f: (A, B) => Goal[R, E]): Unify[R, E, A, B] = f(_, _)
  }
}

object Unifiers {

  private[zkanren] def values[R, E, A: Tag, B: Tag]: Unify[R, E, A, B] = { case (a, b) =>
    val ch = ZChannel.serviceWithChannel[Unify.UMap] { umap =>
      umap.get(Tag[(A, B)].tag) match {
        case Some(u: Unify[R, E, A, B]) => u(a, b).toChannel
        case _                          => Goal.reject.toChannel
      }
    }
    Goal.fromChannel[R, E](ch)
  }

  private[zkanren] def terms[R, E, A: Tag]: Unify[R, E, LTerm[A], LTerm[A]] = { case (a, b) =>
    Goal.fromReadLoop[R, E] { state =>
      val makeChan: ZSTM[R, E, Chan[R, E]] =
        state.unify(a, b).either.map {
          case Right(_)           => ZChannel.write(Right(state))
          case Left(None)         => ZChannel.write(Left(state))
          case Left(Some((a, b))) => values[R, E, A, A].apply(a.value, b.value).toChannel
        }
      ZChannel.unwrap(makeChan.commit)
    }
  }

  private[zkanren] def options[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, Option[A], Option[B]] = {
    case (Some(a), Some(b)) => u(a, b)
    case (None, None)       => Goal.accept
    case _                  => Goal.reject
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

  // Unifies if a is the head of bs.
  private[zkanren] def headOfIterable[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, A, Iterable[B]] = { case (a, bs) => options(u)(Some(a), bs.headOption) }

  // Unifies if as is the tail of bs.
  private[zkanren] def tailOfIterable[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, Iterable[A], Iterable[B]] = { case (as, bs) => iterables(u)(as, bs.tail) }

}
