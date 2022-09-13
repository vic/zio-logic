package zkanren.internal

import zio.stream.{ZChannel, ZPipeline, ZStream}
import zio.{Chunk, Promise, Ref, ZIO, ZLayer}

final class Goal[-R, +E] private[Goal] (private val channel: Goal.Chan[R, E]) extends AnyVal { self =>
  @inline def and[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
    Goal.conj(Seq(this, goal))

  def or[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
    Goal.disj(Seq(this, goal))

  @inline def orElse[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
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

  @inline def toChannel: Goal.Chan[R, E] = channel

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
   * It can be seen as a function: `State => ZIO[R, E, Either[State, State]]`
   */
  type Chan[-R, +E] = ZChannel[R, Any, State, Any, E, Either[State, State], Any]

  def fromChannel[R, E](ch: Chan[R, E]): Goal[R, E] = new Goal(ch)

  def fromFunction[R, E](f: State => Either[State, State]): Goal[R, E] =
    fromReadLoop[R, E](ZChannel.write(_).mapOut(f))

  def fromStream[R, E](s: ZStream[R with State, E, Either[State, State]]): Goal[R, E] = {
    val read = ZChannel.write(_: State).concatMap { state =>
      ZChannel.unit >>> s.toChannel
        .provideSomeLayer[R](ZLayer.succeed(state))
        .concatMap(ZChannel.writeChunk)
    }
    fromReadLoop(read)
  }

  def fromPipeline[R, E](p: ZPipeline[R, E, State, Either[State, State]]): Goal[R, E] = {
    val ch = ZChannel
      .write(_: State)
      .mapOut(Chunk.succeed(_))
      .concatMap(ZChannel.write(_) >>> p.toChannel)
      .concatMap(ZChannel.writeChunk(_))
    fromReadLoop(ch)
  }

  def fromZIO[R, E](f: ZIO[R with State, E, Either[State, State]]): Goal[R, E] =
    fromReadLoop[R, E](ZChannel.write(_).mapOutZIO(s => f.provideSomeLayer[R](ZLayer.succeed(s))))

  def fromZIOPredicate[R, E](predicate: ZIO[R with State, E, Boolean]): Goal[R, E] =
    fromZIO[R, E](ZIO.serviceWithZIO[State](s => predicate.map(Either.cond(_, s, s))))

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
  def neg[R, E](g: Goal[R, E]): Goal[R, E] = peek(swap(succeedOnce(g)))

  // Logical conjunction of goals. Goals are run sequentially as long as their
  // previous one succeeds.
  def conj[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] =
    goals.iterator.reduceLeft[Goal[R, E]](_ pipeSuccessTo _)

  // TODO: parallel conjunction, goals executed in parallel but only emit
  // if all of them have succeeded.
//  def conjPar[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
//    val goalList = goals.iterator.toList
//    val latches = goalList.map(_ => Promise.make[])
//
//    lazy val chunk = Chunk.fromIterator(goals.iterator.map(g => g.channel))
//    val ch         = ZChannel.mergeAll(ZChannel.writeChunk(chunk), n = 16)
//    Goal.fromChannel(ch)
//  }

  // Logical disjunction of goals.
  // Goals are run in parallel with independent states
  // The resulting goal emits as soon as any given goal emits.
  def disj[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = {
    lazy val chunk = Chunk.fromIterator(goals.iterator.map(g => branch.pipeSuccessTo(g).channel))
    val ch         = ZChannel.mergeAll(ZChannel.writeChunk(chunk), n = 16)
    Goal.fromChannel(ch)
  }

  // Creates a goal identical to the given one but that will only ever consume one input.
  // Note that reading once from upstream will cause the stream to terminate.
  def readOnce[R, E](g: Goal[R, E]): Goal[R, E] = {
    val ch = ZChannel.readWithCause[R, Any, State, Any, E, Either[State, State], Any](
      in = ZChannel.write(_) >>> g.toChannel,
      halt = c => ZChannel.failCause(c.stripFailures),
      done = ZChannel.succeed(_)
    )
    fromChannel(ch)
  }

  private def emitOnlyOnce[R, E](
    write: PartialFunction[Either[State, State], ZChannel[Any, Any, Any, Any, Nothing, Either[State, State], Unit]]
  )(g: Goal[R, E]): Goal[R, E] = {
    def ch(done: Boolean): ZChannel[R, E, Either[State, State], Any, E, Either[State, State], Any] =
      ZChannel.readWithCause(
        in = {
          case _ if done                 => ch(done)
          case x if write.isDefinedAt(x) => write(x) *> ch(true)
          case _                         => ch(done)
        },
        halt = ZChannel.failCause(_),
        done = ZChannel.succeed(_)
      )
    fromChannel(g.toChannel >>> ch(false))
  }

  // Creates a goal that is the same as the given goal but only emits once.
  def once[R, E](g: Goal[R, E]): Goal[R, E] =
    emitOnlyOnce(ZChannel.write(_))(g)

  // Creates a goal that is the same as the given goal but succeeds only once.
  def succeedOnce[R, E](g: Goal[R, E]): Goal[R, E]            =
    emitOnlyOnce({ case Right(s) => ZChannel.write(Right(s)) })(g)

  // Creates a goal that is the same as the given goal but fails only once.
  def failOnce[R, E](g: Goal[R, E]): Goal[R, E]               =
    emitOnlyOnce({ case Left(s) => ZChannel.write(Left(s)) })(g)

  // Logical disjunction of goals but only emits once. Useful for potentially expensive
  // goals where we only need evidence of at least one success.
  def race[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = succeedOnce(disj(goals))

}
