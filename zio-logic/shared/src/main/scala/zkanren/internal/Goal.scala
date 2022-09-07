package zkanren.internal

import zio.stream.{ZSink, ZStream}

trait Goal[-R, +E] extends (State => ZStream[R, E, Either[State, State]])

object Goal extends GoalMixin

private[internal] trait GoalMixin extends Fresh with Query with Equal {

  def left[R, E]: Goal[R, E]  = state => ZStream.succeed(Left(state))
  def right[R, E]: Goal[R, E] = state => ZStream.succeed(Right(state))

  def sequence[R, E](onRight: Boolean = true, n: Int = 16)(goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    val onLeft = !onRight
    val it     = goals.iterator

    def cont(s: ZStream[R, E, Either[State, State]]): ZStream[R, E, Either[State, State]] =
      s.flatMap {
        case Right(state) if onRight && it.hasNext => cont(it.next()(state))
        case Left(state) if onLeft && it.hasNext   => cont(it.next()(state))
        case Right(state) if onRight               => right(state)
        case Left(state) if onLeft                 => left(state)
        case e if onRight                          => e.fold(left, left)
        case e if onLeft                           => e.fold(right, right)
      }

    { state => cont(if (onRight) right(state) else left(state)) }
  }

  def conjunctionRight[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] =
    sequence[R, E](onRight = true, n)(goal +: goals)

  def conjunctionLeft[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] =
    sequence[R, E](onRight = false, n)(goal +: goals)

  def disjunction[R, E](n: Int = 16)(goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { state =>
    val all = goal(state) +: goals.map(_(state))
    ZStream.mergeAll(n)(all: _*)
  }

}
