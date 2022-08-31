package zkanren.core

import zkanren.core.Goal.Goal

import scala.language.implicitConversions

trait Unify[-A] {
  def apply[R, E](a: => A, b: => A): Goal[R, E]
}

object Unify {
  implicit def anyValTerm[A <: AnyVal](a: A): LTerm[A] = LVal(a)

  implicit def unifyTerms[A]: Unify[LTerm[A]] = new Unify[LTerm[A]] {
    override def apply[R, E](a: => LTerm[A], b: => LTerm[A]): Goal[R, E] = Goal.unifyTerm[A](a, b)
  }

}
