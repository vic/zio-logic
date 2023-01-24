package zkanren.internal

import zio.stm.ZSTM
import zio.stream.{ZChannel, ZPipeline, ZStream}
import zio.{CanFail, Chunk, Tag, ZIO, ZLayer}

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
    Goal.fromChannel[R1, E1](ch)
  }

  def pipeSuccessTo[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] = {
    val rights = channel.collect { case Right(e) => e }
    val ch     = rights >>> goal.channel
    Goal.fromChannel[R1, E1](ch)
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
    fromReadLoop[R, E](read)
  }

  def fromPipeline[R, E](p: ZPipeline[R, E, State, Either[State, State]]): Goal[R, E] = {
    val ch = ZChannel
      .write(_: State)
      .mapOut(Chunk.succeed(_))
      .concatMap(ZChannel.write(_) >>> p.toChannel)
      .concatMap(ZChannel.writeChunk(_))
    fromReadLoop[R, E](ch)
  }

  def fromZIOAccept[R, E: CanFail](f: ZIO[R, E, Any]): Goal[R, Nothing] =
    unwrap[R, Nothing](f.either.fold(_ => Goal.reject, _ => Goal.accept))

  def fromZIOVal[R, E, A: Tag](z: ZIO[R, E, A])(f: LVal[A] => Goal[R, E]): Goal[R, E] =
    unwrap[R, E](z.map(LVal(_)).map(f))

  def fromZIOTerm[R, E, A](z: ZIO[R, E, LTerm[A]])(f: LTerm[A] => Goal[R, E]): Goal[R, E] =
    unwrap[R, E](z.map(f))

  def fromReadLoop[R, E](read: State => Chan[R, E]): Goal[R, E] = {
    lazy val channel: Chan[R, E] =
      ZChannel.readWithCause(
        in = read(_) *> channel,
        halt = e => ZChannel.failCause(e.stripFailures),
        done = ZChannel.succeedNow(_)
      )
    Goal.fromChannel(channel)
  }

  def unwrap[R, E](z: ZIO[R with State, E, Goal[R, E]]): Goal[R, E] =
    Goal.fromReadLoop[R, E](s =>
      ZChannel.write(s) >>>
        ZChannel.unwrap(z.map(_.toChannel).provideSomeLayer[R](ZLayer.succeed(s)))
    )

  // Swaps a goal exit status, making it fail if successful or the other way around.
  def swap[R, E](g: Goal[R, E]): Goal[R, E] =
    fromChannel[R, E](g.toChannel.mapOut(_.swap))

  // Rejects all success
  def reject: Goal[Any, Nothing] = fromFunction(Left(_))

  // Keeps success as it is, used mainly as fallback case in conde.
  def accept: Goal[Any, Nothing] = fromFunction(Right(_))

  // Creates a new branch of state so that changes are not shared.
  def branch: Goal[Any, Nothing] =
    unwrap(ZSTM.serviceWithSTM[State](_.branch).map(s => Goal.fromChannel(ZChannel.write(Right(s)))).commit)

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
    Goal.fromChannel[R, E](ch)
  }

  // Creates a channel identical to the given one but that will only emit once and then stop reading
  // from upstream events, terminating the whole stream.
  def halt[R, E](g: Goal[R, E]): Goal[R, E] = {
    val ch = ZChannel.readWithCause[R, E, Either[State, State], Any, E, Either[State, State], Any](
      in = ZChannel.write(_),
      halt = ZChannel.failCause(_),
      done = ZChannel.succeed(_)
    )
    fromChannel[R, E](g.toChannel >>> ch)
  }

  // Creates a goal identical to the given one but that will only ever consume one input.
  // Note that reading once from upstream will cause the stream to terminate.
  def readOnce[R, E](g: Goal[R, E]): Goal[R, E] = {
    val ch = ZChannel.readWithCause[R, Any, State, Any, E, Either[State, State], Any](
      in = ZChannel.write(_) >>> g.toChannel,
      halt = c => ZChannel.failCause(c.stripFailures),
      done = ZChannel.succeed(_)
    )
    fromChannel[R, E](ch)
  }

  private def emitOnlyOnce[R, E](
    onRight: Option[Boolean] // None means on either right or left.
  )(g: Goal[R, E]): Goal[R, E] = {
    def ch(done: Boolean): ZChannel[R, E, Either[State, State], Any, E, Either[State, State], Any] =
      ZChannel.readWithCause(
        in = {
          case _ if done                          => ch(done)
          case x if onRight.isEmpty               => ZChannel.write(x) *> ch(true)
          case Right(x) if onRight.contains(true) => ZChannel.write(Right(x)) *> ch(done = true)
          case Left(x) if onRight.contains(false) => ZChannel.write(Left(x)) *> ch(done = true)
          case _                                  => ch(done)
        },
        halt = ZChannel.failCause(_),
        done = ZChannel.succeed(_)
      )
    fromChannel[R, E](g.toChannel >>> ch(false))
  }

  // Creates a goal that is the same as the given goal but only emits once. But waits for upstream to finish.
  def once[R, E](g: Goal[R, E]): Goal[R, E] = emitOnlyOnce(None)(g)

  // Creates a goal that is the same as the given goal but succeeds only once. But waits for upstream to finish.
  def succeedOnce[R, E](g: Goal[R, E]): Goal[R, E] = emitOnlyOnce(Some(true))(g)

  // Creates a goal that is the same as the given goal but fails only once. But waits for upstream to finish.
  def failOnce[R, E](g: Goal[R, E]): Goal[R, E] = emitOnlyOnce(Some(false))(g)

  // Logical disjunction of goals but only emits once. Useful for potentially expensive
  // goals where we only need evidence of at least one success.
  def race[R, E](goals: IterableOnce[Goal[R, E]]): Goal[R, E] = succeedOnce(disj(goals))

}
