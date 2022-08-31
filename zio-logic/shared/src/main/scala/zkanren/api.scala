package zkanren

import zio.Zippable
import zio.stm.{URSTM, ZSTM}

import scala.language.implicitConversions

object api {
  type State     = core.State
  type LTerm[+A] = core.LTerm[A]
  type LVar[+A]  = core.LVar[A]
  type LVal[+A]  = core.LVal[A]

  type Goal[-R, +E] = core.Goal.Goal[R, E]

  lazy val emptyStateLayer = core.State.empty

  @inline def query[V] = core.Goal.query[V] _
  @inline def fresh[V] = core.Goal.fresh[V] _
  @inline def lval[A]  = core.Goal.lval[A] _

  @inline def lvar[A]                          = core.Goal.lvar[A]
  @inline def lvar2[A, B]                      = lvar[A] zip lvar[B]
  @inline def lvar3[A, B, C]                   = lvar2[A, B] zip lvar[C]
  @inline def lvar4[A, B, C, D]                = lvar3[A, B, C] zip lvar[D]
  @inline def lvar5[A, B, C, D, E]             = lvar4[A, B, C, D] zip lvar[E]
  @inline def lvar6[A, B, C, D, E, F]          = lvar5[A, B, C, D, E] zip lvar[F]
  @inline def lvar7[A, B, C, D, E, F, G]       = lvar6[A, B, C, D, E, F] zip lvar[G]
  @inline def lvar8[A, B, C, D, E, F, G, H]    = lvar7[A, B, C, D, E, F, G] zip lvar[H]
  @inline def lvar9[A, B, C, D, E, F, G, H, I] = lvar8[A, B, C, D, E, F, G, H] zip lvar[I]

  implicit class LVarOps[A](private val a: URSTM[State, A]) extends AnyVal {
    @inline def <*>[B](b: URSTM[State, LVar[B]])(implicit
      z: Zippable[A, LVar[B]]
    ): ZSTM[State, Nothing, Zippable[A, LVar[B]]#Out] =
      a.zip[State, Nothing, LVar[B]](b)
  }

  implicit class TermOps[A](private val a: LTerm[A]) extends AnyVal {
    @inline def =:=(b: LTerm[A])(implicit unify: core.Unify[LTerm[A]]): Goal[Any, Nothing] =
      unify.apply[Any, Nothing](a, b)
  }

  implicit class UnifiableOps[A](private val a: A) extends AnyVal {
    @inline def =:=(b: A)(implicit unify: core.Unify[A]): Goal[Any, Nothing] =
      unify.apply[Any, Nothing](a, b)
  }

  implicit class GoalOps[R, E](private val a: core.Goal.Goal[R, E]) extends AnyVal {
    @inline def &&(b: core.Goal.Goal[R, E]) = core.Goal.conjunction()(a, b)
    @inline def ||(b: core.Goal.Goal[R, E]) = core.Goal.disjunction()(a, b)
  }

  implicit def anyValTerm[A <: AnyVal](a: A): LTerm[A] = lval(a)
}
