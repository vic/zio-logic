package zkanren.internal

import zio.stream.{ZChannel, ZStream}
import zio.{Cause, Chunk, UIO, ZIO, ZLayer}

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

  def fromFunction[R, E](f: State => Either[State, State]): Goal[R, E] =
    fromReadLoop[R, E](ZChannel.write(_).mapOut(f))

  def fromZIO[R, E](f: ZIO[R with State, E, Either[State, State]]): Goal[R, E] =
    fromReadLoop[R, E](ZChannel.write(_).mapOutZIO(s => f.provideSomeLayer[R](ZLayer.succeed(s))))

  def fromReadLoop[R, E](read: State => Chan[R, E]): Goal[R, E] = {
    lazy val channel: Chan[R, E] =
      ZChannel.readWithCause(
        in = read(_) *> channel,
        halt = e => ZChannel.failCause(e.stripFailures),
        done = ZChannel.succeedNow(_)
      )
    Goal.fromChannel(channel)
  }

  // Swaps a goal exit status, making it fail if successful or the other way around.
  def swap[R, E](g: Goal[R, E]): Goal[R, E] =
    fromChannel(g.toChannel.mapOut(_.swap))

  // Rejects all success
  def reject: Goal[Any, Nothing] = fromFunction(Left(_))

  // Keeps success as it is, used mainly as fallback case in conde.
  def accept: Goal[Any, Nothing] = fromFunction(Right(_))

  // Creates a new branch of state so that changes are not shared.
  def branch: Goal[Any, Nothing] =
    fromZIO[Any, Nothing](ZIO.serviceWithZIO[State](_.branch.commit.map(Right(_))))

  // Performs a given goal in a temporary state branch and restores to the original state at the end.
  def peek[R, E](g: Goal[R, E]): Goal[R, E] =
    fromReadLoop[R, E] { savedState =>
      val ch = conj(Seq(branch, g)).toChannel.mapOut(_.fold(_ => Left(savedState), _ => Right(savedState)))
      ZChannel.write(savedState) >>> ch
    }

  // Negates a goal. Succeeds if goal fails in a temporary state branch.
  def neg[R, E](g: Goal[R, E]): Goal[R, E] = peek(swap(g))

  // Logical conjunction of goals. Goals are run as long as their previous one succeeds.
  def conj[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] =
    goals.iterator.reduceLeft[Goal[R, E]](_ pipeSuccessTo _)

  // Logical disjunction of goals. Goals are run in parallel as they are independent form one another.
  def disj[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    lazy val chunk = Chunk.fromIterator(goals.iterator.map(_.channel))
    val ch         = ZChannel.mergeAll(ZChannel.writeChunk(chunk), n = 16)
    Goal.fromChannel(ch)
  }

  def race[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    def raceFirst(s: State): ZIO[R, E, Either[State, State]] =
      ZStream
        .fromChannel(ZChannel.write(s) >>> disj(goals).toChannel.mapOut(Chunk.succeed))
        .collectRight
        .runHead
        .map(_.fold[Either[State, State]](Left(s))(Right(_)))
    Goal.fromReadLoop(ZChannel.write(_).mapOutZIO(raceFirst))
  }

  def unifyTerm[A](a: LTerm[A], b: LTerm[A]): Goal[Any, Nothing] =
    fromZIO[Any, Nothing](
      ZIO.serviceWithZIO[State](s =>
        s.bind(a, b)
          .commit
          .either
          .map(_.fold(_ => Left(s), _ => Right(s)))
      )
    )

}
