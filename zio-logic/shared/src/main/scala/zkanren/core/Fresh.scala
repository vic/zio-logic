package zkanren.core

import zio.stm.{URSTM, ZSTM}
import zio.stream.ZStream

private[core] trait Fresh { self: Goal =>
  type T1[T[+_], A]          = T[A]
  type T2[T[+_], A, B]       = (T[A], T[B])
  type T3[T[+_], A, B, C]    = (T[A], T[B], T[C])
  type T4[T[+_], A, B, C, D] = (T[A], T[B], T[C], T[D])

  def lval[A](a: => A): LVal[A]      = LVal(a)
  def lvar[A]: URSTM[State, LVar[A]] = ZSTM.serviceWithSTM[State](_.fresh[A])

  def fresh[V](v: URSTM[State, V]): Fresh.PartiallyApplied[V] = new Fresh.PartiallyApplied[V](v)
  def query[V](v: URSTM[State, V]): Query.PartiallyApplied[V] = new Query.PartiallyApplied[V](v)
}

object Fresh {
  import Goal.Goal

  final class PartiallyApplied[V](private val v: URSTM[State, V]) extends AnyVal {
    def apply[R, E](x: V => Goal[R, E]): Goal[R, E] = ZStream.unwrap(v.map(x).commit)
  }
}

object Query {
  import Goal._

  implicit class Query1[A](private val p: PartiallyApplied[LVar[A]]) {
    def apply[R, E](f: LVar[A] => Goal[R, E]): ZStream[R with State, E, LTerm[A]] =
      p.toStream[R, E, LTerm[A]](Seq(_), { case Seq(a) => a.asInstanceOf[LTerm[A]] })(f)
  }

  implicit class Query2[A, B](p: PartiallyApplied[T2[LVar, A, B]]) {
    def apply[R, E](f: ((LVar[A], LVar[B])) => Goal[R, E]): ZStream[R with State, E, (LTerm[A], LTerm[B])] =
      p.toStream[R, E, (LTerm[A], LTerm[B])](
        { case (a, b) => Seq(a, b) },
        { case Seq(a, b) => (a, b).asInstanceOf[(LTerm[A], LTerm[B])] }
      )(f)
  }
//
//  implicit class Query3[V, A, B, C](p: PartiallyApplied[V])(implicit ev: V <:< T3[LVar, A, B, C]) {
//    def apply[R, E](f: V => Goal[R, E]): ZStream[R with State, E, T3[LTerm, A, B, C]] =
//      p.toStream[R, E, T3[LTerm, A, B, C]](
//        { case (a, b, c) => Seq(a, b, c) },
//        { case Seq(a, b, c) => (a, b, c).asInstanceOf[T3[LTerm, A, B, C]] }
//      )(f)
//  }

  final class PartiallyApplied[V](private[Query] val v: URSTM[State, V]) extends AnyVal {
    def toStream[R, E, O](f: V => Seq[LVar[_]], g: Seq[LTerm[_]] => O)(
      x: V => Goal[R, E]
    ): ZStream[R with State, E, O] =
      ZStream.unwrap(v.map { vars =>
        x(vars).collectRight.mapZIO(_.query(f(vars)).map(g).commit)
      }.commit)
  }
}
