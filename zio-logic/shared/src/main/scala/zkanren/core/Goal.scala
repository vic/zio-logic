package zkanren.core

import zio.stream.ZStream

object Goal              extends Goal
private[core] trait Goal extends Fresh with Equal {
  type Goal[-R, +E] = ZStream[R with State, E, Either[State, State]]

  def conjunction[R, E](goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] =
    goals.foldLeft(goal) { case (prev, next) =>
      prev.flatMapPar(16) {
        case Left(state)  => ZStream.succeed(Left(state))
        case Right(state) => next.updateService[State](_ => state)
      }
    }

  def disjunction[R, E](goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = {
    val all = goal +: goals
    ZStream.mergeAll(n = 16)(all: _*)
  }

}
