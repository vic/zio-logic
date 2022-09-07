package zkanren.core

import zio.stm.{URSTM, ZSTM}
import zio.stream.ZStream
import zio.{ZEnvironment, ZIO, ZLayer}

private[core] trait Fresh { self: Goal =>
  def lval[A](a: => A): LVal[A]      = LVal(a)
  def lvar[A]: URSTM[State, LVar[A]] = ZSTM.serviceWithSTM[State](_.fresh[A])

  def fresh[V](v: URSTM[State, V]): Fresh.PartiallyApplied[V] = new Fresh.PartiallyApplied[V](v)
}

object Fresh {
  import Goal.Goal

  final class PartiallyApplied[V](private val v: URSTM[State, V]) extends AnyVal {
    def apply[R, E](x: V => Goal[R, E]): Goal[R, E] = { state =>
      ZStream.unwrap {
        v.map(x)
          .flatMap(ZSTM.serviceWith[State](_))
          .commit
          .provideSomeLayer(ZLayer.succeed(state))
      }
    }
  }
}
