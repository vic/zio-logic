package zkanren.internal

import zio.ZLayer
import zio.stm.{URSTM, ZSTM}
import zio.stream.ZStream

private[internal] trait Fresh { self: GoalMixin =>
  def lval[A](a: => A): LVal[A]      = LVal(a)
  def lvar[A]: URSTM[State, LVar[A]] = ZSTM.serviceWithSTM[State](_.fresh[A])

  def fresh[V](v: URSTM[State, V]): Fresh.PartiallyApplied[V] = new Fresh.PartiallyApplied[V](v)

//  def defo[V](v: URSTM[State, V]): Fresh.Defo[V] = new Fresh.Defo[V](v)
}

private[internal] object Fresh {
//  final class Defo[V](private val v: URSTM[State, V]) extends AnyVal {}
//  object Defo {
//    implicit def defoUnify[R, E, V](implicit u: Unify[R, E, V, V]): Unify[R, E, V, Defo[V]] = (v1: V, v2: Defo[V]) =>
//      ???
//  }

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
