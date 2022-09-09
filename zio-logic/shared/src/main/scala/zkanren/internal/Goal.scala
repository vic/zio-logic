package zkanren.internal

import zio.stream.{ZChannel, ZStream}
import zio.{Chunk, UIO}

final class Goal[-R, +E] private[Goal] (private val channel: Goal.Chan[R, E]) extends AnyVal { self =>
  @inline def and[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
    pipeSuccessTo(goal)

  def or[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
    Goal.fromChannel(channel.zipParRight(goal.channel))

  def orElse[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
    pipeFailureTo(goal)

  def pipeFailureTo[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] = {
    val lefts = channel.collect { case Left(e) => e }
    val ch    = lefts >>> goal.channel
    Goal.fromChannel(ch)
  }

  def pipeSuccessTo[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] = {
    val rights = channel.collect { case Right(e) => e }
    val ch     = rights >>> goal.channel
    Goal.fromChannel(ch)
  }

  def toChannel: Goal.Chan[R, E] = channel

  def toStream: ZStream[R with State, E, Either[State, State]] = {
    lazy val chan = ZChannel.serviceWithChannel[State] { state =>
      ZChannel.write(state) >>> channel.mapOut(Chunk.succeed)
    }
    ZStream.fromChannel(chan)
  }

}

object Goal {

  /**
   * A goal channel transforms input states into either unified or not-unifiable states.
   */
  type Chan[-R, +E] = ZChannel[R, Any, State, Any, E, Either[State, State], Any]

  def fromChannel[R, E](ch: Chan[R, E]): Goal[R, E] = new Goal(ch)

  def fail[R, E]: Goal[R, E] = {
    lazy val channel: Chan[R, E] = ZChannel.readWithCause[R, Any, State, Any, E, Either[State, State], Any](
      in = ZChannel.write(_).mapOut(Left(_)) *> channel,
      halt = _ => ZChannel.unit,
      done = ZChannel.succeedNow(_)
    )
    Goal.fromChannel(channel)
  }

  def succeed: Goal[Any, Nothing] = {
    lazy val channel: Chan[Any, Nothing] = ZChannel.readWithCause(
      in = ZChannel.write(_).mapOut(Right(_)) *> channel,
      halt = cause => ZChannel.unit,
      done = pill => ZChannel.unit
    )
    Goal.fromChannel(channel)
  }

  def conj[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] =
    goals.iterator.reduceLeft[Goal[R, E]](_ pipeSuccessTo _)

  def disj[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    lazy val chunk = Chunk.fromIterator(goals.iterator.map(_.channel))
    val ch         = ZChannel.mergeAll(ZChannel.writeChunk(chunk), n = 16)
    Goal.fromChannel(ch)
  }

  def unifyTerm[A](a: LTerm[A], b: LTerm[A]): Goal[Any, Nothing] = {
    def unifyEffect(state: State): UIO[Either[State, State]] =
      state.bind(a, b).commit.either

    lazy val channel: Chan[Any, Nothing] =
      ZChannel.readWithCause(
        in = { state => ZChannel.write(state).mapOutZIO(unifyEffect) *> channel },
        halt = _ => ZChannel.unit,
        done = _ => ZChannel.unit
      )

    Goal.fromChannel(channel)
  }

}
