package zkanren.internal

import zio.stream.{ZSink, ZStream}

trait Goal[-R, +E] extends (State => ZStream[R, E, Either[State, State]])

object Goal extends GoalMixin

private[internal] trait GoalMixin extends Fresh with Query with Equal {

  def left[R, E]: Goal[R, E]  = state => ZStream.succeed(Left(state))
  def right[R, E]: Goal[R, E] = state => ZStream.succeed(Right(state))

  def bind[R, E](onRight: Boolean = true, n: Int = 16)(goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    val onLeft = !onRight
    val it     = goals.iterator

    val cont: Either[State, State] => ZStream[R, E, Either[State, State]] = {
      case Right(state) if onRight && it.hasNext => it.next()(state).flatMap(cont)
      case Left(state) if onLeft && it.hasNext   => it.next()(state).flatMap(cont)
      case Right(state) if onRight               => right(state)
      case Left(state) if onLeft                 => left(state)
      case e if onRight                          => e.fold(left, left)
      case e if onLeft                           => e.fold(right, right)
    }

    val initial = if (onRight) right else left
    initial(_).flatMap(cont)
  }

  def conjunctionRight[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] =
    bind[R, E](onRight = true, n)(goal +: goals)

  def conjunctionLeft[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] =
    bind[R, E](onRight = false, n)(goal +: goals)

  def disjunction[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { state =>
    val all = goal(state) +: goals.map(_(state))
    ZStream.mergeAll(n)(all: _*)
  }

}
