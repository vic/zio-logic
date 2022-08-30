package zkanren.core

import zio.{Chunk, ZIO}
import zio.stm.{USTM, ZSTM}
import zio.stream.{ZChannel, ZStream}

object Goal {
  type Goal[R, E] = ZStream[R, E, Either[State, State]] => ZStream[R, E, Either[State, State]]

  def apply[R, E](f: State => ZStream[R, E, Either[State, State]]): Goal[R, E] =
    _.flatMap {
      case Left(state)  => ZStream.succeed(Left(state))
      case Right(state) => f(state)
    }

  def accept[R, E]: Goal[R, E] = identity
  def reject[R, E]: Goal[R, E] = negation

  def negation[R, E]: Goal[R, E] = _.map(_.swap)

  def conjunction[R, E](goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { stream =>
    goals.foldLeft(goal(stream)) { case (prev, goal) =>
      prev.flatMapPar(n = 16) {
        case Left(state)  => ZStream.succeed(Left(state))
        case Right(state) => goal(ZStream.succeed(Right(state)))
      }
    }
  }

  def disjunction[R, E](goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { stream =>
    val streams = (goal +: goals).map(_(stream))
    ZStream.mergeAll(n = 16)(streams: _*)
  }

  def equalTerm[R, E, A](a: Term[A], b: Term[A]): Goal[R, E] = Goal { state =>
    ZStream.fromZIO(state.bind(a, b).commit.either)
  }

  def equal[R, E, A](a: => A, b: => A)(implicit unify: Unify[A]): Goal[R, E] = unify(a, b)

  def equalProduct[R, E, A](a: Iterable[A], b: Iterable[A])(implicit unify: Unify[A]): Goal[R, E] = {
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

  type V1[A]          = Var[A]
  type V2[A, B]       = (Var[A], Var[B])
  type V3[A, B, C]    = (Var[A], Var[B], Var[C])
  type V4[A, B, C, D] = (Var[A], Var[B], Var[C], Var[D])

  def fresh[R, E, A]: (V1[A] => Goal[R, E]) => Goal[R, E] =
    freshN(_.fresh[A])

  def fresh2[R, E, A, B]: (V2[A, B] => Goal[R, E]) => Goal[R, E] =
    freshN(s => s.fresh[A] <*> s.fresh[B])

  def fresh3[R, E, A, B, C]: (V3[A, B, C] => Goal[R, E]) => Goal[R, E] =
    freshN { s =>
      s.fresh[A]
        .zip[Any, Nothing, Var[B]](s.fresh)
        .zip[Any, Nothing, Var[C]](s.fresh)
    }

  def fresh4[R, E, A, B, C, D]: (V4[A, B, C, D] => Goal[R, E]) => Goal[R, E] =
    freshN { s =>
      s.fresh[A]
        .zip[Any, Nothing, Var[B]](s.fresh)
        .zip[Any, Nothing, Var[C]](s.fresh)
        .zip[Any, Nothing, Var[D]](s.fresh)
    }

  def freshN[R, E, T](k: State => USTM[T])(f: T => Goal[R, E]): Goal[R, E] = Goal { state =>
    ZStream.fromZIO(k(state).commit).flatMap(f(_)(ZStream.succeed(Right(state))))
  }

}
