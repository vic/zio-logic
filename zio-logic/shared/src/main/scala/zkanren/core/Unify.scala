package zkanren.core

import zkanren.core.Goal.Goal

trait Unify[-A, -B] {
  def apply[R, E](a: => A, b: => B): Goal[R, E]
}
