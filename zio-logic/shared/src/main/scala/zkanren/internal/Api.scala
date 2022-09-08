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

  @inline def lval[A] = Goal.lval[A] _

  @inline def lvar[A]                          = Goal.lvar[A]
  @inline def lvar2[A, B]                      = lvar[A] zip lvar[B]
  @inline def lvar3[A, B, C]                   = lvar2[A, B] zip lvar[C]
  @inline def lvar4[A, B, C, D]                = lvar3[A, B, C] zip lvar[D]
  @inline def lvar5[A, B, C, D, E]             = lvar4[A, B, C, D] zip lvar[E]
  @inline def lvar6[A, B, C, D, E, F]          = lvar5[A, B, C, D, E] zip lvar[F]
  @inline def lvar7[A, B, C, D, E, F, G]       = lvar6[A, B, C, D, E, F] zip lvar[G]
  @inline def lvar8[A, B, C, D, E, F, G, H]    = lvar7[A, B, C, D, E, F, G] zip lvar[H]
  @inline def lvar9[A, B, C, D, E, F, G, H, I] = lvar8[A, B, C, D, E, F, G, H] zip lvar[I]

  @inline def fresh[V]                          = Goal.fresh[V] _
  @inline def fresh1[A]                         = fresh(lvar[A])
  @inline def fresh2[A, B]                      = fresh(lvar2[A, B])
  @inline def fresh3[A, B, C]                   = fresh(lvar3[A, B, C])
  @inline def fresh4[A, B, C, D]                = fresh(lvar4[A, B, C, D])
  @inline def fresh5[A, B, C, D, E]             = fresh(lvar5[A, B, C, D, E])
  @inline def fresh6[A, B, C, D, E, F]          = fresh(lvar6[A, B, C, D, E, F])
  @inline def fresh7[A, B, C, D, E, F, G]       = fresh(lvar7[A, B, C, D, E, F, G])
  @inline def fresh8[A, B, C, D, E, F, G, H]    = fresh(lvar8[A, B, C, D, E, F, G, H])
  @inline def fresh9[A, B, C, D, E, F, G, H, I] = fresh(lvar9[A, B, C, D, E, F, G, H, I])

  @inline def query[V]                          = Goal.query[V] _
  @inline def query1[A]                         = query(lvar[A])
  @inline def query2[A, B]                      = query(lvar2[A, B])
  @inline def query3[A, B, C]                   = query(lvar3[A, B, C])
  @inline def query4[A, B, C, D]                = query(lvar4[A, B, C, D])
  @inline def query5[A, B, C, D, E]             = query(lvar5[A, B, C, D, E])
  @inline def query6[A, B, C, D, E, F]          = query(lvar6[A, B, C, D, E, F])
  @inline def query7[A, B, C, D, E, F, G]       = query(lvar7[A, B, C, D, E, F, G])
  @inline def query8[A, B, C, D, E, F, G, H]    = query(lvar8[A, B, C, D, E, F, G, H])
  @inline def query9[A, B, C, D, E, F, G, H, I] = query(lvar9[A, B, C, D, E, F, G, H, I])

  @inline implicit def unifiableOps[A]: A => Api.UnifiableOps[A]      = Api.UnifiableOps[A] _
  @inline implicit def goalOps[R, E]: Goal[R, E] => Api.GoalOps[R, E] = Api.GoalOps[R, E] _

  @inline implicit def unifyTerms[A]: Unify[Any, Nothing, LTerm[A], LTerm[A]] = Unify.terms[A]

  @inline implicit def unifyIterables[R, E, A, B](implicit
    u: Unify[R, E, A, B]
  ): Unify[R, E, IterableOnce[A], IterableOnce[B]] =
    Unify.iterables[R, E, A, B]
}
