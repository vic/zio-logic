package zkanren.core

import zio.stm.{URSTM, ZSTM}
import zio.stream.ZStream

private[core] trait Query { self: GoalMixin =>
  def query[V](v: URSTM[State, V]): Query.PartiallyApplied[V] = new Query.PartiallyApplied[V](v)
}

object Query {

  implicit class Query1[A](private val p: PartiallyApplied[LVar[A]]) {
    def apply[R, E](f: LVar[A] => Goal[R, E]): ZStream[R with State, E, LTerm[A]] =
      p.toStream[R, E, LTerm[A]](Seq(_), { case Seq(a) => a.asInstanceOf[LTerm[A]] })(f)
  }

  implicit class Query2[A, B](p: PartiallyApplied[(LVar[A], LVar[B])]) {
    def apply[R, E](f: ((LVar[A], LVar[B])) => Goal[R, E]): ZStream[R with State, E, (LTerm[A], LTerm[B])] =
      p.toStream[R, E, (LTerm[A], LTerm[B])](
        { case (a, b) => Seq(a, b) },
        { case Seq(a, b) => (a, b).asInstanceOf[(LTerm[A], LTerm[B])] }
      )(f)
  }

  implicit class Query3[A, B, C](p: PartiallyApplied[(LVar[A], LVar[B], LVar[C])]) {
    def apply[R, E](
      f: ((LVar[A], LVar[B], LVar[C])) => Goal[R, E]
    ): ZStream[R with State, E, (LTerm[A], LTerm[B], LTerm[C])] =
      p.toStream[R, E, (LTerm[A], LTerm[B], LTerm[C])](
        { case (a, b, c) => Seq(a, b, c) },
        { case Seq(a, b, c) => (a, b, c).asInstanceOf[(LTerm[A], LTerm[B], LTerm[C])] }
      )(f)
  }

  implicit class Query4[A, B, C, D](p: PartiallyApplied[(LVar[A], LVar[B], LVar[C], LVar[D])]) {
    def apply[R, E](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D])) => Goal[R, E]
    ): ZStream[R with State, E, (LTerm[A], LTerm[B], LTerm[C], LTerm[D])] =
      p.toStream[R, E, (LTerm[A], LTerm[B], LTerm[C], LTerm[D])](
        { case (a, b, c, d) => Seq(a, b, c, d) },
        { case Seq(a, b, c, d) => (a, b, c, d).asInstanceOf[(LTerm[A], LTerm[B], LTerm[C], LTerm[D])] }
      )(f)
  }

  implicit class Query5[A, B, C, D, E](p: PartiallyApplied[(LVar[A], LVar[B], LVar[C], LVar[D], LVar[E])]) {
    def apply[R, X](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E])) => Goal[R, X]
    ): ZStream[R with State, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E])] =
      p.toStream[R, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E])](
        { case (a, b, c, d, e) => Seq(a, b, c, d, e) },
        { case Seq(a, b, c, d, e) => (a, b, c, d, e).asInstanceOf[(LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E])] }
      )(f)
  }

  implicit class Query6[A, B, C, D, E, F](p: PartiallyApplied[(LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F])]) {
    def apply[R, X](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F])) => Goal[R, X]
    ): ZStream[R with State, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F])] =
      p.toStream[R, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F])](
        { case (a, b, c, d, e, f) => Seq(a, b, c, d, e, f) },
        { case Seq(a, b, c, d, e, f) =>
          (a, b, c, d, e, f).asInstanceOf[(LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F])]
        }
      )(f)
  }

  implicit class Query7[A, B, C, D, E, F, G](
    p: PartiallyApplied[(LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F], LVar[G])]
  ) {
    def apply[R, X](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F], LVar[G])) => Goal[R, X]
    ): ZStream[R with State, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G])] =
      p.toStream[R, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G])](
        { case (a, b, c, d, e, f, g) => Seq(a, b, c, d, e, f, g) },
        { case Seq(a, b, c, d, e, f, g) =>
          (a, b, c, d, e, f, g).asInstanceOf[(LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G])]
        }
      )(f)
  }

  implicit class Query8[A, B, C, D, E, F, G, H](
    p: PartiallyApplied[(LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F], LVar[G], LVar[H])]
  ) {
    def apply[R, X](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F], LVar[G], LVar[H])) => Goal[R, X]
    ): ZStream[R with State, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G], LTerm[H])] =
      p.toStream[R, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G], LTerm[H])](
        { case (a, b, c, d, e, f, g, h) => Seq(a, b, c, d, e, f, g, h) },
        { case Seq(a, b, c, d, e, f, g, h) =>
          (a, b, c, d, e, f, g, h)
            .asInstanceOf[(LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G], LTerm[H])]
        }
      )(f)
  }

  implicit class Query9[A, B, C, D, E, F, G, H, I](
    p: PartiallyApplied[(LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F], LVar[G], LVar[H], LVar[I])]
  ) {
    def apply[R, X](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F], LVar[G], LVar[H], LVar[I])) => Goal[R, X]
    ): ZStream[
      R with State,
      X,
      (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G], LTerm[H], LTerm[I])
    ] =
      p.toStream[R, X, (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G], LTerm[H], LTerm[I])](
        { case (a, b, c, d, e, f, g, h, i) => Seq(a, b, c, d, e, f, g, h, i) },
        { case Seq(a, b, c, d, e, f, g, h, i) =>
          (a, b, c, d, e, f, g, h, i)
            .asInstanceOf[(LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F], LTerm[G], LTerm[H], LTerm[I])]
        }
      )(f)
  }

  final class PartiallyApplied[V](private[Query] val makeVars: URSTM[State, V]) extends AnyVal {
    def toStream[R, E, O](f: V => Seq[LVar[_]], g: PartialFunction[Seq[LTerm[_]], O])(
      makeGoal: V => Goal[R, E]
    ): ZStream[R with State, E, O] = {
      val m = for {
        state <- ZSTM.service[State]
        vars  <- makeVars
        stream = makeGoal(vars)(state).collectRight.mapZIO(_.query(f(vars)).map(g).commit)
      } yield stream
      ZStream.unwrap(m.commit)
    }

  }
}
