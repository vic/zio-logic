package zkanren.internal

import zio.stm.{URSTM, ZSTM}
import zio.stream.ZChannel
import zio.{ZIO, ZLayer}

private[internal] object Fresh {
  def lvar[A]: URSTM[State, LVar[A]]                          = ZSTM.serviceWithSTM[State](_.fresh[A])
  def fresh[V](v: URSTM[State, V]): Fresh.PartiallyApplied[V] = new Fresh.PartiallyApplied[V](v)

//  final class Defo[V](private val v: URSTM[State, V]) extends AnyVal {}
//  object Defo {
//    implicit def defoUnify[R, E, V](implicit u: Unify[R, E, V, V]): Unify[R, E, V, Defo[V]] = (v1: V, v2: Defo[V]) =>
//      ???
//  }

  final class PartiallyApplied[V](private val v: URSTM[State, V]) extends AnyVal {
    def apply[R, E](x: V => Goal[R, E]): Goal[R, E] = {
      def makeGoal(state: State): ZIO[Any, Nothing, Goal[R, E]] =
        v.map(x).commit.provideSomeLayer(ZLayer.succeed(state))

      lazy val channel: Goal.Chan[R, E] = ZChannel.readWithCause(
        in = s => ZChannel.unwrap(makeGoal(s).map(goal => ZChannel.write(s) >>> goal.toChannel)),
        halt = e => ZChannel.failCause(e.stripFailures),
        done = ZChannel.succeedNow(_)
      )

      Goal.fromChannel(channel)
    }
  }
}
