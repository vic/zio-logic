package zkanren.core

import zio.stream.ZStream

object Goal              extends Goal
private[core] trait Goal extends Fresh with Query with Equal {
  type Goal[-R, +E] = ZStream[R with State, E, Either[State, State]]

  def conjunction[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] =
    goals.foldLeft(goal) { case (prev, next) =>
      prev.flatMapPar(n) {
        case Left(state)  => ZStream.fromZIO(state.branch.map(Left(_)).commit)
        case Right(state) => next.updateService[State](_ => state)
      }
    }

  def disjunction[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = {
    val all = goal +: goals
    ZStream.mergeAll(n)(all: _*)
  }

}
