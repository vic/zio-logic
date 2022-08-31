package zkanren.core

sealed trait LTerm[+A]
final case class LVar[+A] private[core] (intern: Long) extends LTerm[A]

final class LVal[+A] private[LVal] (thunk: () => A) extends LTerm[A] {
  def apply(): A = thunk()
}
object LVal {
  def apply[A](a: => A): LVal[A] = new LVal(() => a)
}
