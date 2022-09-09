package zkanren.internal

import zio.ULayer
import zkanren.internal

private[zkanren] trait Api extends Api.Exports with Api.FreshQuery with Api.MicroKanren
private[zkanren] object Api {
  implicit class UnifyOps[+A](private val a: A) extends AnyVal {
    @inline def =:=[R, E, B](b: B)(implicit unify: Unify[R, E, A, B]): Goal[R, E] = unify(a, b)
  }

  implicit class GoalOps[-R, +E](private val self: Goal[R, E]) {
    @inline def &&[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
      self and goal

    @inline def ||[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
      self or goal
  }

  trait Exports {
    type State     = internal.State
    type LTerm[+A] = internal.LTerm[A]
    type LVar[+A]  = internal.LVar[A]
    type LVal[+A]  = internal.LVal[A]

    type Unify1[-R, +E, -A]    = Unify[R, E, A, A]
    type Unify[-R, +E, -A, -B] = internal.Unify[R, E, A, B]

    type Goal[-R, +E] = internal.Goal[R, E]

    lazy val Unify = internal.Unify

    lazy val emptyStateLayer: ULayer[State]                = State.empty
    @inline implicit def unifyOps[A]: A => Api.UnifyOps[A] = Api.UnifyOps[A] _
    @inline implicit def goalOps[R, E]                     = Api.GoalOps[R, E] _

    @inline implicit def unifyTerms[A]: Unify[Any, Nothing, LTerm[A], LTerm[A]] = Unify.terms[A]

    @inline implicit def unifyIterables[R, E, A, B](implicit
      u: Unify[R, E, A, B]
    ): Unify[R, E, IterableOnce[A], IterableOnce[B]] =
      Unify.iterables[R, E, A, B]
  }

  trait FreshQuery {
    @inline def lval[A] = LVal[A] _

    @inline def lvar[A]                          = Fresh.lvar[A]
    @inline def lvar2[A, B]                      = lvar[A] zip lvar[B]
    @inline def lvar3[A, B, C]                   = lvar2[A, B] zip lvar[C]
    @inline def lvar4[A, B, C, D]                = lvar3[A, B, C] zip lvar[D]
    @inline def lvar5[A, B, C, D, E]             = lvar4[A, B, C, D] zip lvar[E]
    @inline def lvar6[A, B, C, D, E, F]          = lvar5[A, B, C, D, E] zip lvar[F]
    @inline def lvar7[A, B, C, D, E, F, G]       = lvar6[A, B, C, D, E, F] zip lvar[G]
    @inline def lvar8[A, B, C, D, E, F, G, H]    = lvar7[A, B, C, D, E, F, G] zip lvar[H]
    @inline def lvar9[A, B, C, D, E, F, G, H, I] = lvar8[A, B, C, D, E, F, G, H] zip lvar[I]

    @inline def fresh[V]                          = Fresh.fresh[V] _
    @inline def fresh1[A]                         = fresh(lvar[A])
    @inline def fresh2[A, B]                      = fresh(lvar2[A, B])
    @inline def fresh3[A, B, C]                   = fresh(lvar3[A, B, C])
    @inline def fresh4[A, B, C, D]                = fresh(lvar4[A, B, C, D])
    @inline def fresh5[A, B, C, D, E]             = fresh(lvar5[A, B, C, D, E])
    @inline def fresh6[A, B, C, D, E, F]          = fresh(lvar6[A, B, C, D, E, F])
    @inline def fresh7[A, B, C, D, E, F, G]       = fresh(lvar7[A, B, C, D, E, F, G])
    @inline def fresh8[A, B, C, D, E, F, G, H]    = fresh(lvar8[A, B, C, D, E, F, G, H])
    @inline def fresh9[A, B, C, D, E, F, G, H, I] = fresh(lvar9[A, B, C, D, E, F, G, H, I])

    @inline def query[V]                          = Query.query[V] _
    @inline def query1[A]                         = query(lvar[A])
    @inline def query2[A, B]                      = query(lvar2[A, B])
    @inline def query3[A, B, C]                   = query(lvar3[A, B, C])
    @inline def query4[A, B, C, D]                = query(lvar4[A, B, C, D])
    @inline def query5[A, B, C, D, E]             = query(lvar5[A, B, C, D, E])
    @inline def query6[A, B, C, D, E, F]          = query(lvar6[A, B, C, D, E, F])
    @inline def query7[A, B, C, D, E, F, G]       = query(lvar7[A, B, C, D, E, F, G])
    @inline def query8[A, B, C, D, E, F, G, H]    = query(lvar8[A, B, C, D, E, F, G, H])
    @inline def query9[A, B, C, D, E, F, G, H, I] = query(lvar9[A, B, C, D, E, F, G, H, I])
  }

  trait MicroKanren {
    def conj[R, E](g: Goal[R, E], gs: Goal[R, E]*): Goal[R, E] =
      Goal.conj[R, E](g +: gs)

    def disj[R, E](g: Goal[R, E], gs: Goal[R, E]*): Goal[R, E] =
      Goal.disj[R, E](g +: gs)

    def conde[R, E](cases: IterableOnce[Goal[R, E]]*): Goal[R, E] =
      Goal.disj[R, E](cases.iterator.map(Goal.conj[R, E]))

  }
}
