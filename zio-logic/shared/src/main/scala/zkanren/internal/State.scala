package zkanren.internal

import zio.stm.{STM, TRef, URSTM, USTM, ZSTM}
import zio.{ULayer, ZLayer}

import scala.annotation.tailrec

sealed trait State {
  def fresh[A]: USTM[LVar[A]]
  private[internal] def unify[A](v: LTerm[A], t: LTerm[A]): STM[Option[(LVal[A], LVal[A])], Unit]
  def reify[A](v: LTerm[A]): USTM[LTerm[A]]
  def branch: USTM[State]
}

private[internal] object State {
  import BindingOps.Bindings

  def empty: ULayer[State] = ZLayer.fromZIO(emptyState().commit)

  def reifyNow[A](v: LTerm[A]): URSTM[State, LTerm[A]] =
    ZSTM.serviceWithSTM[State](_.reify[A](v))

  // Reifies a variable until it can be resolved into a value.
  def reifyEventually[A](v: LTerm[A]): URSTM[State, A] =
    reifyNow[A](v).flatMap {
      case lVal: LVal[A] => ZSTM.succeed(lVal.value)
      case x             => ZSTM.fail(x)
    }.eventually

  private def make(nextVar: TRef[Long], bindings: TRef[Bindings]): State = new State {
    override def fresh[A]: USTM[LVar[A]] =
      nextVar.getAndUpdate(_ + 1).map(LVar(_))

    override def unify[A](v: LTerm[A], t: LTerm[A]): STM[Option[(LVal[A], LVal[A])], Unit] =
      bindings.modify(BindingOps.bind(v, t)).flatMap {
        case Left(BindingOps.MutualReference)         => STM.fail(None)
        case Right(None)                              => STM.unit
        case Right(Some(BindingOps.UnequalVal(a, b))) => STM.fail(Some(a -> b))
      }

    override def reify[A](v: LTerm[A]): USTM[LTerm[A]] =
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

    private[State] def bind[A](x: LTerm[A], y: LTerm[A])(
      bindings: Bindings
    ): (Either[MutualReference.type, Option[UnequalVal[A]]], Bindings) =
      bindable[A](x, y)(bindings) match {
        case AlreadySame      => Right(None)                   -> bindings
        case Assignable(v, t) => Right(None)                   -> (bindings + (v -> t))
        case MutualReference  => Left(MutualReference)         -> bindings
        case UnequalVal(a, b) => Right(Some(UnequalVal(a, b))) -> bindings
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

    sealed trait Bindable[+A]
    case object MutualReference                       extends Bindable[Nothing]
    case object AlreadySame                           extends Bindable[Nothing]
    case class Assignable[A](v: LVar[A], t: LTerm[A]) extends Bindable[A]
    case class UnequalVal[A](a: LVal[A], b: LVal[A])  extends Bindable[A]

    private def bindable[A](a: LTerm[A], b: LTerm[A])(bindings: Bindings): Bindable[A] = {
      val (aVal, aSeen) = walk(a, Seq(a))(bindings)
      val (bVal, bSeen) = walk(b, Seq(b))(bindings)

      (aVal, bVal) match {
        case (x: LVar[A], y: LVar[A]) if aSeen.contains(y) || bSeen.contains(x) => MutualReference

        case (x: LVar[A], y: LVar[A]) if x.variable == y.variable => AlreadySame
        case (x: LVal[A], y: LVal[A]) if x.value == y.value       => AlreadySame
        case (x: LVal[A], y: LVal[A])                             => UnequalVal(x, y)

        case (x: LVar[A], y: LVar[A]) => Assignable(x, y)
        case (x: LVar[A], y: LVal[A]) => Assignable(x, y)
        case (x: LVal[A], y: LVar[A]) => Assignable(y, x)
      }
    }

  }
}
