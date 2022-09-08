package zkanren.internal

sealed trait LTerm[+A]

final class LVar[+A] private[internal] (val intern: Long) extends LTerm[A]
private[internal] object LVar {
  def apply[A](n: Long): LVar[A] = new LVar[A](n)
}

final class LVal[+A] private[LVal] (thunk: () => A) extends LTerm[A] {
  private[this] lazy val value = thunk()

  def apply(): A = value
}
private[internal] object LVal {
  def apply[A](a: A): LVal[A] = new LVal(() => a)
}
