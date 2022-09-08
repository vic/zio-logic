package zkanren.internal

import zio.Chunk
import zio.stream.{ZSink, ZStream}

trait Goal[-R, +E] extends (State => ZStream[R, E, Either[State, State]])

private[internal] object Goal extends GoalMixin

private[internal] trait GoalMixin extends Fresh with Query with Equal {

  def left[R, E]: Goal[R, E]  = state => ZStream.succeed(Left(state))
  def right[R, E]: Goal[R, E] = state => ZStream.succeed(Right(state))

  def mplus[R, E](n: Int = 16)(goals: IterableOnce[Goal[R, E]]): Goal[R, E] = { state =>
    ZStream.fromChunk(Chunk.fromIterator(goals.iterator)).flatMapPar(n)(_(state))
  }

  def bind[R, E](onRight: Boolean = true, n: Int = 16)(goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    val onLeft = !onRight
    val it     = goals.iterator

    lazy val cont: Either[State, State] => ZStream[R, E, Either[State, State]] = {
      case Right(state) if onRight && it.hasNext => it.next()(state).flatMapPar(n)(cont)
      case Left(state) if onLeft && it.hasNext   => it.next()(state).flatMapPar(n)(cont)
      case Right(state) if onRight               => right(state)
      case Left(state) if onLeft                 => left(state)
      case e if onRight                          => e.fold(left, left)
      case e if onLeft                           => e.fold(right, right)
    }

    val initial = if (onRight) right else left
    initial(_).flatMapPar(n)(cont)
  }

  def bindStream[R, E](onRight: Boolean = true, n: Int = 16)(s: ZStream[R, E, Goal[R, E]]): Goal[R, E] = {
    val onLeft  = !onRight
    val initial = if (onRight) right else left
    initial(_).flatMapPar(n) {
      case Right(state) if onRight => s.flatMapPar(n)(_(state))
      case Left(state) if onLeft   => s.flatMapPar(n)(_(state))
      case Right(state)            => right(state)
      case Left(state)             => left(state)
    }
  }

}
