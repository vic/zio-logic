package zkanren.internal

import zio.ULayer
import zkanren.internal

private[zkanren] object Api {
  implicit class UnifiableOps[+A](private val a: A) extends AnyVal {
    @inline def =:=[R, E, B](b: B)(implicit unify: Unify[R, E, A, B]): Goal[R, E] = unify(a, b)
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

  type Unify1[-R, +E, -A]    = Unify[R, E, A, A]
  type Unify[-R, +E, -A, -B] = internal.Unify[R, E, A, B]

  type Goal[-R, +E] = internal.Goal[R, E]

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

  @inline implicit def unifiableOps[A]: A => Api.UnifiableOps[A]      = Api.UnifiableOps[A] _
  @inline implicit def goalOps[R, E]: Goal[R, E] => Api.GoalOps[R, E] = Api.GoalOps[R, E] _

  @inline implicit def unifyTerms[A]: Unify[Any, Nothing, LTerm[A], LTerm[A]] = Unify.terms[A]

  @inline implicit def unifyIterables[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, IterableOnce[A], IterableOnce[B]] =
    Unify.iterables[R, E, A, B]
}
