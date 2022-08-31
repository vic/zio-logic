package zkanren.core

import zio.ZIO
import zio.stm.ZSTM
import zio.stream.ZStream

private[core] trait Equal { self: Goal =>

  def equalTerm[R, E, A](a: Term[A], b: Term[A]): Goal[R, E] = Goal { state =>
    ZStream.fromZIO(state.bind(a, b).commit.either).debug("EQUAL")
  }

  def equal[R, E, A](a: => A, b: => A)(implicit unify: Unify[A]): Goal[R, E] = unify(a, b)

  def equalIterable[R, E, A](a: Iterable[A], b: Iterable[A])(implicit unify: Unify[A]): Goal[R, E] = {
    val sameLength: Goal[R, E]         = equalTerm(Val(a.size), Val(b.size))
    val equalElements: Seq[Goal[R, E]] = a.zip(b).map { case (a, b) => unify[R, E](a, b) }.toSeq
    conjunction(sameLength, equalElements: _*)
  }

  def equalCommit[R, E, A](a: ZSTM[R, E, A], b: ZSTM[R, E, A])(implicit unify: Unify[A]): Goal[R, E] = Goal { state =>
    ZStream.fromZIO(a.zip(b).commit.either).flatMap {
      case Left(e)       => ZStream.succeed(Left(state))
      case Right((a, b)) => unify[R, E](a, b)(ZStream.succeed(Right(state)))
    }
  }

  def equalStream[R, E, A](a: ZStream[R, E, A], b: ZStream[R, E, A])(implicit unify: Unify[A]): Goal[R, E] = Goal {
    state =>
      a.zipWith(b) { case (a, b) => unify[R, E](a, b) }.either.flatMap {
        case Left(e)     => ZStream.succeed(Left(state))
        case Right(goal) => goal(ZStream.succeed(Right(state)))
      }
  }

  def equalZIO[R, E, A](a: ZIO[R, E, A], b: ZIO[R, E, A])(implicit unify: Unify[A]): Goal[R, E] =
    equalStream(ZStream.fromZIO(a), ZStream.fromZIO(b))

}
