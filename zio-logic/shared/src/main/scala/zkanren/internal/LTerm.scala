package zkanren.internal

sealed trait LTerm[+A]

final class LVar[+A] private[LVar] (val variable: Long) extends LTerm[A]
private[internal] object LVar {
  def apply[A](n: Long): LVar[A] = new LVar[A](n)
}

final class LVal[+A] private[LVal] (val value: A) extends LTerm[A]

private[internal] object LVal {
  def apply[A](a: A): LVal[A] = new LVal(a)
}
