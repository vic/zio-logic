package zkanren.internal

import zio.stm.{STM, TRef, URSTM, USTM, ZSTM}
import zio.{ULayer, ZLayer}

import scala.annotation.tailrec

sealed trait State {
  def fresh[A]: USTM[LVar[A]]
  def unify[A](v: LTerm[A], t: LTerm[A]): STM[(LTerm[A], LTerm[A]), (LTerm[A], LTerm[A])]
  def reify[A](v: LVar[A]): USTM[LTerm[A]]
  def branch: USTM[State]
}

private[internal] object State {
  import BindingOps.Bindings

  def empty: ULayer[State] = ZLayer.fromZIO(emptyState().commit)

  def reifyNow[A](v: LVar[A]): URSTM[State, LTerm[A]] =
    ZSTM.serviceWithSTM[State](_.reify[A](v))

  // Reifies a variable until it can be resolved into a value.
  def reifyEventually[A](v: LVar[A]): URSTM[State, LVal[A]] =
    reifyNow[A](v).flatMap {
      case lVal: LVal[A] => ZSTM.succeed(lVal)
      case lVar: LVar[A] => ZSTM.fail(lVar)
    }.eventually

  private def make(nextVar: TRef[Long], bindings: TRef[Bindings]): State = new State {
    override def fresh[A]: USTM[LVar[A]] =
      nextVar.getAndUpdate(_ + 1).map(LVar(_))

    override def unify[A](v: LTerm[A], t: LTerm[A]): STM[(LTerm[A], LTerm[A]), (LTerm[A], LTerm[A])] =
      bindings
        .modify(b =>
          BindingOps.bind(v, t)(b) match {
            case Left(binds)  =>
              Left((v -> t)) -> binds
            case Right(binds) =>
              Right((v -> t)) -> binds
          }
        )
        .absolve

    override def reify[A](v: LVar[A]): USTM[LTerm[A]] =
      bindings.get.map { bindings =>
        val (r, _) = BindingOps.walk(v, Nil)(bindings)
        r
      }

    override def branch: USTM[State] = for {
      currVar   <- nextVar.get
      currBinds <- bindings.get
      nextVar   <- TRef.make[Long](currVar)
      bindings  <- TRef.make[Bindings](currBinds)
    } yield make(nextVar, bindings)
  }

  private def emptyState(): USTM[State] =
    for {
      nextVar  <- TRef.make[Long](0)
      bindings <- TRef.make[Bindings](Map.empty)
    } yield make(nextVar, bindings)

  private[State] object BindingOps {
    type Bindings = Map[LVar[_], LTerm[_]]

    private[State] def bind[A](x: LTerm[A], y: LTerm[A])(bindings: Bindings): Either[Bindings, Bindings] =
      bindable(x, y)(bindings) match {
        case Right((x, y)) => Right(bindings + (x -> y))
        case Left(Some(_)) => Right(bindings)
        case Left(None)    => Left(bindings)
      }

    @tailrec
    private[State] def walk[A](t: LTerm[A], seen: Seq[LTerm[A]])(bindings: Bindings): (LTerm[A], Seq[LTerm[A]]) =
      t match {
        case x: LVar[A] =>
          bindings.get(x) match {
            case Some(t: LVal[A @unchecked]) => (t, seen)
            case Some(y: LVar[A @unchecked]) => walk(y, y +: seen)(bindings)
            case _                           => (x, seen)
          }
        case _          => (t, seen)
      }

    // Right means the terms are bindable.
    // Left(Some) means the terms are equal (==) and no binding needs to be done.
    // Left(None) means the terms are not equal and cannot be bound.
    private type Bindable[A] = Either[Option[LTerm[A]], (LVar[A], LTerm[A])]
    private def bindable[A](a: LTerm[A], b: LTerm[A])(bindings: Bindings): Bindable[A] = {
      val (aVal, aSeen) = walk(a, Seq(a))(bindings)
      val (bVal, bSeen) = walk(b, Seq(b))(bindings)

      (aVal, bVal) match {
        case (x: LVar[A], y: LVar[A]) if aSeen.contains(y) || bSeen.contains(x) => Left(None)

        case (x: LVar[A], y: LVar[A]) if x.variable == y.variable => Left(Some(x))
        case (x: LVal[A], y: LVal[A]) if x.value == y.value       => Left(Some(x))

        case (x: LVar[A], y: LVar[A]) => Right(x -> y)
        case (x: LVar[A], y: LVal[A]) => Right(x -> y)
        case (x: LVal[A], y: LVar[A]) => Right(y -> x)
        case _                        => Left(None)
      }
    }

  }
}
