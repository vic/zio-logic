package zkanren.core

import zio.stream.ZStream

object Goal              extends Goal
private[core] trait Goal extends Fresh with Equal {
  type Goal[R, E] = ZStream[R, E, Either[State, State]] => ZStream[R, E, Either[State, State]]

  def apply[R, E](f: State => ZStream[R, E, Either[State, State]]): Goal[R, E] =
    _.flatMap {
      case Left(state)  => ZStream.succeed(Left(state))
      case Right(state) => f(state)
    }

  def branch[R, E]: Goal[R, E] = Goal { state =>
    ZStream.fromZIO(state.branch.map(Right(_)).commit)
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

}
