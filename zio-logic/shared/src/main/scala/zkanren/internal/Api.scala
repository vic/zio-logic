package zkanren.internal

import zio.stm.ZSTM
import zio.{Duration, Tag, ULayer, ZIO, ZLayer}
import zkanren.internal

private[zkanren] trait Api extends Api.Exports with Api.FreshQuery with Api.Micro
private[zkanren] object Api {
//  implicit def swapUnify[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, B, A] = { case (b, a) => u(a, b) }

//  implicit def tuple1Unify[R, E, A0, A1](implicit u1: Unify[R, E, A0, A1]): Unify[R, E, Tuple1[A0], Tuple1[A1]] = {
//    case (l, r) => l._1 =:= r._1
//  }
//
//  implicit def tuple2Unify[R, X, A0, A1, B0, B1](implicit
//    u1: Unify[R, X, A0, A1],
//    u2: Unify[R, X, B0, B1]
//  ): Unify[R, X, (A0, B0), (A1, B1)] = { case (l, r) =>
//    Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2))
//  }
//
//  implicit def tuple3Unify[R, X, A0, A1, B0, B1, C0, C1](implicit
//    u1: Unify[R, X, A0, A1],
//    u2: Unify[R, X, B0, B1],
//    u3: Unify[R, X, C0, C1]
//  ): Unify[R, X, (A0, B0, C0), (A1, B1, C1)] = { case (l, r) =>
//    Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3))
//  }
//
//  implicit def tuple4Unify[R, X, A0, A1, B0, B1, C0, C1, D0, D1](implicit
//    u1: Unify[R, X, A0, A1],
//    u2: Unify[R, X, B0, B1],
//    u3: Unify[R, X, C0, C1],
//    u4: Unify[R, X, D0, D1]
//  ): Unify[R, X, (A0, B0, C0, D0), (A1, B1, C1, D1)] = { case (l, r) =>
//    Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3, l._4 =:= r._4))
//  }
//
//  implicit def tuple5Unify[R, X, A0, A1, B0, B1, C0, C1, D0, D1, E0, E1](implicit
//    u1: Unify[R, X, A0, A1],
//    u2: Unify[R, X, B0, B1],
//    u3: Unify[R, X, C0, C1],
//    u4: Unify[R, X, D0, D1],
//    u5: Unify[R, X, E0, E1]
//  ): Unify[R, X, (A0, B0, C0, D0, E0), (A1, B1, C1, D1, E1)] = { case (l, r) =>
//    Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3, l._4 =:= r._4, l._5 =:= r._5))
//  }
//
//  implicit def tuple6Unify[R, X, A0, A1, B0, B1, C0, C1, D0, D1, E0, E1, F0, F1](implicit
//    u1: Unify[R, X, A0, A1],
//    u2: Unify[R, X, B0, B1],
//    u3: Unify[R, X, C0, C1],
//    u4: Unify[R, X, D0, D1],
//    u5: Unify[R, X, E0, E1],
//    u6: Unify[R, X, F0, F1]
//  ): Unify[R, X, (A0, B0, C0, D0, E0, F0), (A1, B1, C1, D1, E1, F1)] = { case (l, r) =>
//    Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3, l._4 =:= r._4, l._5 =:= r._5, l._6 =:= r._6))
//  }

  implicit class UnifyOps[+A](private val a: A) extends AnyVal {
    @inline def =:=[R, E, B](b: B)(implicit unify: Unify[R, E, A, B]): Goal[R, E] = unify(a, b)

    @inline def =!=[R, E, B](b: B)(implicit unify: Unify[R, E, A, B]): Goal[R, E] = Goal.neg(unify(a, b))
  }

  implicit class GoalOps[-R, +E](private val self: Goal[R, E]) {
    @inline def &&[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
      self and goal

    @inline def ||[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
      self or goal

    @inline def &>[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
      self pipeSuccessTo goal

    @inline def |>[R1 <: R, E1 >: E](goal: Goal[R1, E1]): Goal[R1, E1] =
      self pipeFailureTo goal

    @inline def >>[R1 <: R, E1 >: E](f: Goal[R, E] => Goal[R1, E1]): Goal[R1, E1] =
      f(self)

  }

  implicit class GoalFOps[+IR, -IE, -OR, +OE](private val f: Goal[IR, IE] => Goal[OR, OE]) {
    @inline def <<(g: Goal[IR, IE]): Goal[OR, OE] = f(g)
  }

  trait Exports {
    object eventually extends Eventually
    object rel        extends Relations

    type State     = internal.State
    type LTerm[+A] = internal.LTerm[A]
    type LVar[+A]  = internal.LVar[A]
    type LVal[+A]  = internal.LVal[A]

    type Unify1[-R, +E, -A]    = Unify[R, E, A, A]
    type Unify[-R, +E, -A, -B] = internal.Unify[R, E, A, B]
    type U[-A]                 = internal.Unify.U[A]
    type UMap                  = internal.Unify.UMap

    type Goal[-R, +E] = internal.Goal[R, E]
    lazy val Goal = internal.Goal

    lazy val Unify = internal.Unify

    lazy val emptyStateLayer: ULayer[State]                = State.empty ++ ZLayer.succeed[UMap](Map.empty)
    @inline implicit def unifyOps[A]: A => Api.UnifyOps[A] = Api.UnifyOps[A] _
    @inline implicit def goalOps[R, E]                     = Api.GoalOps[R, E] _

    @inline implicit def unifyTerms[R: Tag, E: Tag, A: Tag] = Unifiers.terms[R, E, A]

//    @inline implicit def unifyIterables[R, E, A, B](implicit
//      u: Unify[R, E, A, B]
//    ): Unify[R, E, IterableOnce[A], IterableOnce[B]] =
//      Unifiers.iterables[R, E, A, B]
  }

  trait FreshQuery {
    @inline def lval[A] = LVal[A] _

    @inline def lvar[A]                          = Fresh.lvar[A]
    @inline def lvar1[A]                         = Fresh.lvar[A]
    @inline def lvar2[A, B]                      = lvar[A] zip lvar[B]
    @inline def lvar3[A, B, C]                   = lvar2[A, B] zip lvar[C]
    @inline def lvar4[A, B, C, D]                = lvar3[A, B, C] zip lvar[D]
    @inline def lvar5[A, B, C, D, E]             = lvar4[A, B, C, D] zip lvar[E]
    @inline def lvar6[A, B, C, D, E, F]          = lvar5[A, B, C, D, E] zip lvar[F]
    @inline def lvar7[A, B, C, D, E, F, G]       = lvar6[A, B, C, D, E, F] zip lvar[G]
    @inline def lvar8[A, B, C, D, E, F, G, H]    = lvar7[A, B, C, D, E, F, G] zip lvar[H]
    @inline def lvar9[A, B, C, D, E, F, G, H, I] = lvar8[A, B, C, D, E, F, G, H] zip lvar[I]

    @inline def fresh[V]                          = Fresh.fresh[V] _
    @inline def fresh1[A]                         = fresh(lvar[A])
    @inline def fresh2[A, B]                      = fresh(lvar2[A, B])
    @inline def fresh3[A, B, C]                   = fresh(lvar3[A, B, C])
    @inline def fresh4[A, B, C, D]                = fresh(lvar4[A, B, C, D])
    @inline def fresh5[A, B, C, D, E]             = fresh(lvar5[A, B, C, D, E])
    @inline def fresh6[A, B, C, D, E, F]          = fresh(lvar6[A, B, C, D, E, F])
    @inline def fresh7[A, B, C, D, E, F, G]       = fresh(lvar7[A, B, C, D, E, F, G])
    @inline def fresh8[A, B, C, D, E, F, G, H]    = fresh(lvar8[A, B, C, D, E, F, G, H])
    @inline def fresh9[A, B, C, D, E, F, G, H, I] = fresh(lvar9[A, B, C, D, E, F, G, H, I])

    @inline def query[V]                                = Query.query[V] _
    @inline def query1[R, X, A]                         = query(lvar[A]).apply[R, X]
    @inline def query2[R, X, A, B]                      = query(lvar2[A, B]).apply[R, X]
    @inline def query3[R, X, A, B, C]                   = query(lvar3[A, B, C]).apply[R, X]
    @inline def query4[R, X, A, B, C, D]                = query(lvar4[A, B, C, D]).apply[R, X]
    @inline def query5[R, X, A, B, C, D, E]             = query(lvar5[A, B, C, D, E]).apply[R, X]
    @inline def query6[R, X, A, B, C, D, E, F]          = query(lvar6[A, B, C, D, E, F]).apply[R, X]
    @inline def query7[R, X, A, B, C, D, E, F, G]       = query(lvar7[A, B, C, D, E, F, G]).apply[R, X]
    @inline def query8[R, X, A, B, C, D, E, F, G, H]    = query(lvar8[A, B, C, D, E, F, G, H]).apply[R, X]
    @inline def query9[R, X, A, B, C, D, E, F, G, H, I] = query(lvar9[A, B, C, D, E, F, G, H, I]).apply[R, X]

//    def termo1[R, X, A](f: LVar[A] => Goal[R, X]): LTerm[A] => Goal[R with Unify.U[A], X] =
//      (t: LTerm[A]) => fresh1[A](v => v =:= t && f(v))
//
//    def termo2[R, X, A, B](
//      f: ((LVar[A], LVar[B])) => Goal[R, X]
//    ): (LTerm[A], LTerm[B]) => Goal[R with Unify.U[A] with Unify.U[B], X] =
//      (a, b) => fresh2[A, B](v => v =:= (a, b) && f(v))
//
//    def termo3[R, X, A, B, C](
//      f: ((LVar[A], LVar[B], LVar[C])) => Goal[R, X]
//    )(implicit
//      uA: Unify[R, X, A, A],
//      uB: Unify[R, X, B, B],
//      uC: Unify[R, X, C, C]
//    ): (LTerm[A], LTerm[B], LTerm[C]) => Goal[R, X] =
//      (a, b, c) => fresh3[A, B, C](v => v =:= (a, b, c) && f(v))
//
//    def termo4[R, X, A, B, C, D](
//      f: ((LVar[A], LVar[B], LVar[C], LVar[D])) => Goal[R, X]
//    )(implicit
//      uA: Unify[R, X, A, A],
//      uB: Unify[R, X, B, B],
//      uC: Unify[R, X, C, C],
//      uD: Unify[R, X, D, D]
//    ): (LTerm[A], LTerm[B], LTerm[C], LTerm[D]) => Goal[R, X] =
//      (a, b, c, d) => fresh4[A, B, C, D](v => v =:= (a, b, c, d) && f(v))
//
//    def termo5[R, X, A, B, C, D, E](
//      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E])) => Goal[R, X]
//    )(implicit
//      uA: Unify[R, X, A, A],
//      uB: Unify[R, X, B, B],
//      uC: Unify[R, X, C, C],
//      uD: Unify[R, X, D, D],
//      uE: Unify[R, X, E, E]
//    ): (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E]) => Goal[R, X] =
//      (a, b, c, d, e) => fresh5[A, B, C, D, E](v => v =:= (a, b, c, d, e) && f(v))
//
//    def termo6[R, X, A, B, C, D, E, F](
//      m: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F])) => Goal[R, X]
//    )(implicit
//      uA: Unify[R, X, A, A],
//      uB: Unify[R, X, B, B],
//      uC: Unify[R, X, C, C],
//      uD: Unify[R, X, D, D],
//      uE: Unify[R, X, E, E],
//      uF: Unify[R, X, F, F]
//    ): (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F]) => Goal[R, X] =
//      (a, b, c, d, e, f) => fresh6[A, B, C, D, E, F](v => v =:= (a, b, c, d, e, f) && m(v))
//
  }

  trait Eventually {
    def stmSucceed(v: ZSTM[State, Any, Any], timeout: Duration = Duration.Infinity): Goal[Any, Nothing] =
      Goal.fromZIOPredicate[Any, Nothing](v.eventually.commit.timeout(timeout).map(_.isDefined))

    def satisfyZIO[R, E, A](v: LVar[A], timeout: Duration = Duration.Infinity)(
      f: A => ZIO[R, E, Boolean]
    ): Goal[R, E] =
      Goal.fromZIOPredicate[R, E](
        State
          .reifyEventually(v)
          .commit
          .flatMap[R with State, E, Boolean](f)
          .timeout(timeout)
          .map(_.getOrElse(false))
      )

    def satisfy[A](v: LVar[A], timeout: Duration = Duration.Infinity)(
      predicate: A => Boolean
    ): Goal[Any, Nothing] = satisfyZIO(v, timeout)(a => ZIO.succeed(predicate(a)))

//    def unify[R, E, A, B](
//      a: LTerm[A],
//      b: LTerm[B],
//      timeout: Duration = Duration.Infinity
//    ): Goal[Unify[Any, Nothing, A, B] with Unify[Any, Nothing, A, A] with Unify[Any, Nothing, B, B] with R, E] = {
//      import zkanren.{fresh2, all}
//      fresh2[A, B] { case (vA: LVar[A], vB: LVar[B]) =>
//        def eff(s: State): ZIO[R, Nothing, Chan[R, E]] = State
//          .reifyEventually(vA)
//          .zip(State.reifyEventually(vB))
//          .flatMap { case (a, b) =>
//            ZSTM.serviceWith[Unify[Any, Nothing, A, B]](_(a, b).toChannel)
//          }
//          .commit
//          .timeout(timeout)
//          .map(_.getOrElse(ZChannel.write(Left(s))))
//          .provideSomeLayer[R](ZLayer.succeed(s))
//
//        all(
//          a =:= vA,
//          b =:= vB,
//          Goal.fromReadLoop(s => ZChannel.unwrap(eff(s)))
//        )
//      }
//    }

  }

  trait Micro {
    def ![R, E](g: Goal[R, E]): Goal[R, E] = Goal.neg(g)

    def all[R, E](g: Goal[R, E], gs: Goal[R, E]*): Goal[R, E] =
      Goal.conj[R, E](g +: gs)

    def any[R, E](g: Goal[R, E], gs: Goal[R, E]*): Goal[R, E] =
      Goal.disj[R, E](g +: gs)

    def conde[R, E](cases: IterableOnce[Goal[R, E]]*): Goal[R, E] =
      Goal.disj[R, E](cases.iterator.map(Goal.conj[R, E]))
  }

//  implicit class LOptionOps[A](private val self: LTerm[Option[A]]) extends AnyVal {
//    import zkanren._
//    def isEmpty: Goal[Any, Nothing] = self =:= lval(None)
//
//    def contains[R, E, B](item: LTerm[B])(implicit u: Unify[R, E, A, B]): Goal[Any, Nothing] =
//      ???
//  }

//  implicit class LSeqOps[A](private val self: LTerm[Seq[A]]) extends AnyVal {
//    import zkanren._
//    def isEmpty: Goal[Any, Nothing] = self =:= lval(Nil)
//
//    def reifyEventually: ZSTM[State, Nothing, Seq[LVal[A]]] =
//      State.reifyEventually(self).map(_.map(lval))
//
//    def hasHead[R, E, B](head: LTerm[B])(implicit u: Unify[R, E, LTerm[A], LTerm[B]]): Goal[R, E] = {
//      val x: Seq[LTerm[A]] = ??? // unwrap self
//      // TODO: Obtain only first without having to materialize whole list.
//      x.headOption.map(h => h =:= head).getOrElse(Goal.reject)
//    }
//
//  }

  trait Relations {}

}
