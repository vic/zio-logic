package zkanren.internal

import zio.ULayer
import zkanren.internal

private[zkanren] object Api {
  implicit class UnifiableOps[+A](private val a: A) extends AnyVal {
    @inline def =:=[B](b: B)(implicit unify: Unify[A, B]): Goal[Any, Nothing] =
      unify.apply[Any, Nothing](a, b)
  }

  implicit class GoalOps[R, E](private val a: Goal[R, E]) extends AnyVal {
    @inline def ||(b: Goal[R, E]) = Goal.disjunction()(a, b)
    @inline def &&(b: Goal[R, E]) = Goal.conjunctionRight()(a, b)
    @inline def !!(b: Goal[R, E]) = Goal.conjunctionLeft()(a, b)
  }
}

private[zkanren] trait Api {

  type State     = internal.State
  type LTerm[+A] = internal.LTerm[A]
  type LVar[+A]  = internal.LVar[A]
  type LVal[+A]  = internal.LVal[A]

  type Unify1[-A]    = Unify[A, A]
  type Unify[-A, -B] = internal.Unify[A, B]
  type Goal[-R, +E]  = internal.Goal[R, E]

  lazy val Unify = internal.Unify

  lazy val emptyStateLayer: ULayer[State] = State.empty

  @inline def query[V] = Goal.query[V] _
  @inline def fresh[V] = Goal.fresh[V] _
  @inline def lval[A]  = Goal.lval[A] _

  @inline def lvar[A]                          = Goal.lvar[A]
  @inline def lvar2[A, B]                      = lvar[A] zip lvar[B]
  @inline def lvar3[A, B, C]                   = lvar2[A, B] zip lvar[C]
  @inline def lvar4[A, B, C, D]                = lvar3[A, B, C] zip lvar[D]
  @inline def lvar5[A, B, C, D, E]             = lvar4[A, B, C, D] zip lvar[E]
  @inline def lvar6[A, B, C, D, E, F]          = lvar5[A, B, C, D, E] zip lvar[F]
  @inline def lvar7[A, B, C, D, E, F, G]       = lvar6[A, B, C, D, E, F] zip lvar[G]
  @inline def lvar8[A, B, C, D, E, F, G, H]    = lvar7[A, B, C, D, E, F, G] zip lvar[H]
  @inline def lvar9[A, B, C, D, E, F, G, H, I] = lvar8[A, B, C, D, E, F, G, H] zip lvar[I]

  implicit def unifyTerms[A]: Unify[LTerm[A], LTerm[A]]       = Unify.terms[A]
  implicit def unifiableOps[A]: A => Api.UnifiableOps[A]      = Api.UnifiableOps[A] _
  implicit def goalOps[R, E]: Goal[R, E] => Api.GoalOps[R, E] = Api.GoalOps[R, E] _

  implicit def unifyIterables[A, B](implicit u: Unify[A, B]): Unify[IterableOnce[A], IterableOnce[B]] =
    Unify.iterables[A, B]
}
