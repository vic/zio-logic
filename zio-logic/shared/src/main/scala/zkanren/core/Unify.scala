package zkanren.core

import zkanren.core.Goal.Goal

import scala.language.implicitConversions

trait Unify[-A] {
  def apply[R, E](a: => A, b: => A): Goal[R, E]
}

object Unify {
  implicit def anyValTerm[A <: AnyVal](a: A): Term[A] = Val(a)

  def terms[A]: Unify[Term[A]] = new Unify[Term[A]] {
    override def apply[R, E](a: => Term[A], b: => Term[A]): Goal[R, E] = Goal.equalTerm[R, E, A](a, b)
  }

}
