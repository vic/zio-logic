package zkanren.core

sealed trait Term[+A]
final case class Var[+A] private[core] (intern: Long) extends Term[A]

final class Val[+A] private[Val] (thunk: () => A) extends Term[A] {
  def apply(): A = thunk()
}
object Val {
  def apply[A](a: => A): Val[A] = new Val(() => a)
}
