package zkanren.internal

import zio.stream.ZStream

trait Goal[-R, +E] extends (State => ZStream[R, E, Either[State, State]])

object Goal                       extends GoalMixin
private[internal] trait GoalMixin extends Fresh with Query with Equal {

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
