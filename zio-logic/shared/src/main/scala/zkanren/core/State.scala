package zkanren.core

import zio.stm.{STM, TRef, USTM}

import scala.annotation.tailrec

trait State {
  def fresh[A]: USTM[Var[A]]
  def bind[A](v: Term[A], t: Term[A]): STM[State, State]
  def query(qs: Seq[Term[_]]): USTM[Seq[Term[_]]]
}

object State {
  import BindingOps.Bindings

  def apply(): USTM[State] =
    for {
      nextVar  <- TRef.make[Long](0)
      bindings <- TRef.make[Bindings](Map.empty)
    } yield new State {
      override def fresh[A]: USTM[Var[A]] =
        nextVar.getAndUpdate(_ + 1).map(Var(_))

      override def bind[A](v: Term[A], t: Term[A]): STM[State, State] =
        bindings
          .modify(b =>
            BindingOps.bind(v, t)(b) match {
              case Left(binds)  => Left(this)  -> binds
              case Right(binds) => Right(this) -> binds
            }
          )
          .absolve

      override def query(qs: Seq[Term[_]]): USTM[Seq[Term[_]]] =
        bindings.get.map { bindings =>
          qs.foldLeft[Seq[Term[_]]](Nil) { case (acc, q) =>
            val (r, _) = BindingOps.walk(q, Nil)(bindings)
            acc :+ r
          }
        }
    }

  private[State] object BindingOps {
    type Bindings = Map[Var[_], Term[_]]

    private[State] def bind[A](x: Term[A], y: Term[A])(bindings: Bindings): Either[Bindings, Bindings] =
      bindable(x, y)(bindings) match {
        case Right((x, y)) => Right(bindings + (x -> y))
        case Left(Some(_)) => Right(bindings)
        case Left(None)    => Left(bindings)
      }

    @tailrec
    private[State] def walk[A](t: Term[A], seen: Seq[Term[A]])(bindings: Bindings): (Term[A], Seq[Term[A]]) =
      t match {
        case x: Var[A] =>
          bindings.get(x) match {
            case Some(y: Var[A] @unchecked) => walk(y, y +: seen)(bindings)
            case _                          => (x, seen)
          }
        case _         => (t, seen)
      }

    // Right means the terms are bindable.
    // Left(Some) means the terms are equal (==) and no binding needs to be done.
    // Left(None) means the terms are not equal and cannot be bound.
    private type Bindable[A] = Either[Option[Term[A]], (Var[A], Term[A])]
    private def bindable[A](a: Term[A], b: Term[A])(bindings: Bindings): Bindable[A] = {
      val (aVal, aSeen) = walk(a, Seq(a))(bindings)
      val (bVal, bSeen) = walk(b, Seq(b))(bindings)

      (aVal, bVal) match {
        case (x: Var[A], y: Var[A]) if aSeen.contains(y) || bSeen.contains(x) => Left(None)

        case (x: Var[A], y: Var[A]) if x == y     => Left(Some(x))
        case (x: Val[A], y: Val[A]) if x() == y() => Left(Some(x))

        case (x: Var[A], y: Var[A]) => Right(x -> y)
        case (x: Var[A], y: Val[A]) => Right(x -> y)
        case (x: Val[A], y: Var[A]) => Right(y -> x)
        case _                      => Left(None)
      }
    }

  }
}
