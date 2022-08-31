package zkanren.core

import zio.stm.{URSTM, USTM, ZSTM}
import zio.stream.ZStream

private[core] trait Fresh { self: Goal =>

  type T1[T[+_], A]          = T[A]
  type T2[T[+_], A, B]       = (T[A], T[B])
  type T3[T[+_], A, B, C]    = (T[A], T[B], T[C])
  type T4[T[+_], A, B, C, D] = (T[A], T[B], T[C], T[D])

  def lval[A](a: => A): LVal[A]      = LVal(a)
  def lvar[A]: URSTM[State, LVar[A]] = ZSTM.serviceWithSTM[State](_.fresh[A])

  def fresh[R, E, V](v: URSTM[State, V])(x: V => Goal[R, E]): Goal[R, E] =
    ZStream.unwrap(v.map(x).commit)

  def query[R, E, V, O](v: URSTM[State, V], f: V => Seq[LVar[_]], g: Seq[LTerm[_]] => O)(
    x: V => Goal[R, E]
  ): ZStream[R with State, E, O] =
    ZStream.unwrap(v.map { vars =>
      x(vars).collectRight.mapZIO(_.query(f(vars)).map(g).commit)
    }.commit)

  def query1[R, E, A]: (LVar[A] => Goal[R, E]) => ZStream[R with State, E, T1[LTerm, A]]                         =
    query[R, E, T1[LVar, A], T1[LTerm, A]](lvar[A], Seq(_), { case Seq(a) => a.asInstanceOf[LTerm[A]] })

  def query2[R, E, A, B]: (((LVar[A], LVar[B])) => Goal[R, E]) => ZStream[R with State, E, (LTerm[A], LTerm[B])] =
    query[R, E, T2[LVar, A, B], T2[LTerm, A, B]](
      lvar[A] <*> lvar[B],
      { case (a, b) => Seq(a, b) },
      { case Seq(a, b) => (a, b).asInstanceOf }
    )

}
