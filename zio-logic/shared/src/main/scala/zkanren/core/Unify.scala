package zkanren.core

trait Unify[-A, -B] {
  def apply[R, E](a: => A, b: => B): Goal[R, E]
}
