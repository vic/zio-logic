package zkanren.internal

import zio.stm.{STM, TRef, URSTM, USTM, ZSTM}
import zio.{Tag, ULayer, ZLayer}

import scala.annotation.tailrec

sealed trait State {
  def fresh[A]: USTM[LVar[A]]
  private[internal] def unify[A: Tag, B: Tag](v: LTerm[A], t: LTerm[B]): STM[State.Unbindable, Unit]
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

    override def unify[A: Tag, B: Tag](v: LTerm[A], t: LTerm[B]): STM[Unbindable, Unit] =
      bindings.modify(BindingOps.bind(v, t)).flatMap {
        case None             => STM.unit
        case Some(unbindable) => STM.fail(unbindable)
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

  sealed trait BindTest
  case object AlreadySame                                 extends BindTest
  case class Assign[A](v: LVar[A], t: LTerm[A])           extends BindTest
  sealed trait Unbindable                                 extends BindTest
  case object MutualReference                             extends Unbindable
  case class UnequalTerms[A, B](a: LTerm[A], b: LTerm[B]) extends Unbindable

  private[internal] object BindingOps {
    type Bindings = Map[LVar[_], LTerm[_]]

    private[State] def bind[A: Tag, B: Tag](x: LTerm[A], y: LTerm[B])(
      bindings: Bindings
    ): (Option[Unbindable], Bindings) =
      bindTest(x, y)(bindings) match {
        case AlreadySame            => None             -> bindings
        case Assign(v, t)           => None             -> (bindings + (v -> t))
        case unbindable: Unbindable => Some(unbindable) -> bindings
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

    private def bindTest[A: Tag, B: Tag](a: LTerm[A], b: LTerm[B])(bindings: Bindings): BindTest = {
      val (aTag, bTag) = (Tag[A].tag, Tag[B].tag)

      val (aVal, aSeen) = walk(a, Seq(a))(bindings)
      val (bVal, bSeen) = walk(b, Seq(b))(bindings)

      (aVal, bVal) match {
        case (x: LVal[_], y: LVal[_]) if x.value == y.value       => AlreadySame
        case (x: LVar[_], y: LVar[_]) if x.variable == y.variable => AlreadySame

        case (x, y) if aSeen.contains(y) || bSeen.contains(x) => MutualReference

        case (x: LVar[A], y: LTerm[B]) if aTag <:< bTag => Assign(x, y)
        case (x: LTerm[A], y: LVar[B]) if bTag <:< aTag => Assign(y, x)

        case (x, y) => UnequalTerms(x, y)
      }
    }

  }
}
