package zkanren.core

import zio.stream.ZStream

object Goal              extends Goal
private[core] trait Goal extends Fresh with Query with Equal {
  type Goal[-R, +E] = State => ZStream[R, E, Either[State, State]]

  def conjunctionRight[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { state =>
    goals.foldLeft(goal(state)) { case (prev, next) =>
      prev.flatMapPar(n) {
        case Left(state)  => ZStream.succeed(Left(state))
        case Right(state) => next(state)
      }
    }
  }

  def conjunctionLeft[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { state =>
    goals.foldLeft(goal(state)) { case (prev, next) =>
      prev.flatMapPar(n) {
        case Left(state)  => next(state)
        case Right(state) => ZStream.succeed(Left(state))
      }
    }
  }

  def disjunction[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { state =>
    val all = goal(state) +: goals.map(_(state))
    ZStream.mergeAll(n)(all: _*)
  }

}
