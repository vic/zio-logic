package zkanren

object api {
  type State     = core.State
  type LTerm[+A] = core.LTerm[A]
  type LVar[+A]  = core.LVar[A]
  type LVal[+A]  = core.LVal[A]

  type Goal[-R, +E] = core.Goal.Goal[R, E]

  lazy val emptyStateLayer = core.State.empty

  @inline def lvar[A]  = core.Goal.lvar[A]
  @inline def lval[A]  = core.Goal.lval[A] _
  @inline def query[V] = core.Goal.query[V] _
  @inline def fresh[V] = core.Goal.fresh[V] _

  implicit class TermOps[A](private val a: LTerm[A]) extends AnyVal {
    def =:=(b: LTerm[A])(implicit unify: core.Unify[LTerm[A]]): Goal[Any, Nothing] =
      unify.apply[Any, Nothing](a, b)
  }

  implicit class UnifiableOps[A](private val a: A) extends AnyVal {
    def =:=(b: A)(implicit unify: core.Unify[A]): Goal[Any, Nothing] =
      unify.apply[Any, Nothing](a, b)
  }

  implicit class GoalOps[R, E](private val a: core.Goal.Goal[R, E]) extends AnyVal {
    def &&(b: core.Goal.Goal[R, E]) = core.Goal.conjunction(a, b)
    def ||(b: core.Goal.Goal[R, E]) = core.Goal.disjunction(a, b)
  }

}
