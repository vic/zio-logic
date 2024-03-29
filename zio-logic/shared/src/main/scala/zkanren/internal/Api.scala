package zkanren.internal

import zio.stm.ZSTM
import zio.stream.ZChannel
import zio.{Duration, Tag, ULayer, ZIO, ZLayer}
import zkanren.internal

private[zkanren] trait Api extends Api.Exports with Api.FreshQuery with Api.Micro with Api.Implicits with Api.Reify
private[zkanren] object Api {
//  implicit def swapUnify[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, B, A] = { case (b, a) => u(a, b) }

  trait Implicits {
    implicit def unifyAnyVal[A <: AnyVal]: Unify[Any, Nothing, A, A] = Unify.identity[A]

    implicit def unifyLTerm[R, E, A: Tag, B: Tag](implicit u: Unify[R, E, A, B]): Unify[R, E, LTerm[A], LTerm[B]] =
      Unify.terms[R, E, A, B] {
        case (a: LVal[A], b: LVal[B]) => u(a.value, b.value)
        case _                        => Goal.reject
      }

    implicit val unifyString: Unify[Any, Nothing, String, String] = Unify.identity[String]

    implicit def unifyIterables[R, E, A, B](implicit
      u: Unify[R, E, A, B]
    ): Unify[R, E, IterableOnce[A], IterableOnce[B]] = Unify.iterables[R, E, A, B](u)

    implicit def unifyEither[R, E, L0, L1, R0, R1](implicit
      ul: Unify[R, E, L0, L1],
      ur: Unify[R, E, R0, R1]
    ): Unify[R, E, Either[L0, R0], Either[L1, R1]] = { case (a, b) =>
      a.fold(al => b.fold(bl => ul(al, bl), br => Goal.reject), ar => b.fold(bl => Goal.reject, br => ur(ar, br)))
    }

    implicit def tuple1Unify[R, E, A0, A1](implicit u1: Unify[R, E, A0, A1]): Unify[R, E, Tuple1[A0], Tuple1[A1]] = {
      case (l, r) => l._1 =:= r._1
    }

    implicit def tuple2Unify[R, X, A0, A1, B0, B1](implicit
      u1: Unify[R, X, A0, A1],
      u2: Unify[R, X, B0, B1]
    ): Unify[R, X, (A0, B0), (A1, B1)] = { case (l, r) =>
      Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2))
    }

    implicit def tuple3Unify[R, X, A0, A1, B0, B1, C0, C1](implicit
      u1: Unify[R, X, A0, A1],
      u2: Unify[R, X, B0, B1],
      u3: Unify[R, X, C0, C1]
    ): Unify[R, X, (A0, B0, C0), (A1, B1, C1)] = { case (l, r) =>
      Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3))
    }

    implicit def tuple4Unify[R, X, A0, A1, B0, B1, C0, C1, D0, D1](implicit
      u1: Unify[R, X, A0, A1],
      u2: Unify[R, X, B0, B1],
      u3: Unify[R, X, C0, C1],
      u4: Unify[R, X, D0, D1]
    ): Unify[R, X, (A0, B0, C0, D0), (A1, B1, C1, D1)] = { case (l, r) =>
      Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3, l._4 =:= r._4))
    }

    implicit def tuple5Unify[R, X, A0, A1, B0, B1, C0, C1, D0, D1, E0, E1](implicit
      u1: Unify[R, X, A0, A1],
      u2: Unify[R, X, B0, B1],
      u3: Unify[R, X, C0, C1],
      u4: Unify[R, X, D0, D1],
      u5: Unify[R, X, E0, E1]
    ): Unify[R, X, (A0, B0, C0, D0, E0), (A1, B1, C1, D1, E1)] = { case (l, r) =>
      Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3, l._4 =:= r._4, l._5 =:= r._5))
    }

    implicit def tuple6Unify[R, X, A0, A1, B0, B1, C0, C1, D0, D1, E0, E1, F0, F1](implicit
      u1: Unify[R, X, A0, A1],
      u2: Unify[R, X, B0, B1],
      u3: Unify[R, X, C0, C1],
      u4: Unify[R, X, D0, D1],
      u5: Unify[R, X, E0, E1],
      u6: Unify[R, X, F0, F1]
    ): Unify[R, X, (A0, B0, C0, D0, E0, F0), (A1, B1, C1, D1, E1, F1)] = { case (l, r) =>
      Goal.conj(Seq(l._1 =:= r._1, l._2 =:= r._2, l._3 =:= r._3, l._4 =:= r._4, l._5 =:= r._5, l._6 =:= r._6))
    }

    implicit class LeftTerm[A](private val a: LTerm[A]) {
      def =:=[R, E, B](b: LTerm[B])(implicit u: Unify[R, E, LTerm[A], LTerm[B]]): Goal[R, E] = u(a, b)
      def =:=[R, E, B](b: B)(implicit u: Unify[R, E, LTerm[A], LTerm[B]]): Goal[R, E]        = u(a, LVal(b))
    }

    implicit class LeftVal[A](private val a: A) {
      def =:=[R, E, B](b: B)(implicit u: Unify[R, E, A, B]): Goal[R, E]                      = u(a, b)
      def =:=[R, E, B](b: LTerm[B])(implicit u: Unify[R, E, LTerm[A], LTerm[B]]): Goal[R, E] = u(LVal(a), b)
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

  }

  trait Exports {
    object rel extends Relations

    type State     = internal.State
    type LTerm[+A] = internal.LTerm[A]
    type LVar[+A]  = internal.LVar[A]
    type LVal[+A]  = internal.LVal[A]

    type Unify1[-R, +E, -A]    = Unify[R, E, A, A]
    type Unify[-R, +E, -A, -B] = internal.Unify[R, E, A, B]

    type Goal[-R, +E] = internal.Goal[R, E]
    lazy val Goal = internal.Goal

    lazy val Unify = internal.Unify

    val emptyStateLayer: ULayer[State] = State.empty
  }

  trait Reify {

    def printTerm[A](t: LTerm[A]): Goal[Any, Nothing] =
      reify(t)(x =>
        Goal.fromZIOAccept(x match {
          case x: LVar[A] => zio.Console.printLine(s"LVar#${x.variable}:${x.tag.tag.repr}")
          case x: LVal[A] => zio.Console.printLine(s"LVal(${x})")
        })
      )

    def printVal[A](t: LTerm[A]): Goal[Any, Nothing] =
      reifyVal(t)(a => Goal.fromZIOAccept(zio.Console.printLine(s"LVal(${a})")))

    def reify[R, E, A](t: LTerm[A])(g: LTerm[A] => Goal[R, E]): Goal[R, E] = {
      val eff =
        State
          .reifyNow[A](t)
          .map(g)

      Goal.unwrap[R, E](eff.commit)
    }

    def reifyVar[R, E, A](t: LTerm[A])(g: LVar[A] => Goal[R, E]): Goal[R, E] = {
      val eff =
        State
          .reifyNow[A](t)
          .flatMap {
            case lVar: LVar[A] => ZSTM.succeed(g(lVar))
            case lVal: LVal[A] =>
              ZSTM.serviceWithSTM[State](_.fresh[A]).flatMap { av =>
                ZSTM
                  .serviceWithSTM[State](_.unify[A, A](lVal, av))
                  .fold(_ => Goal.reject, _ => g(av))
              }
          }

      Goal.unwrap[R, E](eff.commit)
    }

    def reifyVal[R, E, A](t: LTerm[A], timeout: Duration = Duration.Infinity)(g: A => Goal[R, E]): Goal[R, E] = {
      val eff =
        State
          .reifyEventually[A](t)
          .map(g)
          .commit
          .timeout(timeout)
          .map(_.getOrElse(Goal.reject))

      Goal.unwrap[R, E](eff)
    }
  }

  trait FreshQuery {
    @inline def lval[A] = LVal[A] _

    @inline def lvar[A]                          = Fresh.lvar[A]
    @inline def lvar1[A]                         = lvar[A]
    @inline def lvar2[A, B]                      = lvar1[A] zip lvar1[B]
    @inline def lvar3[A, B, C]                   = lvar2[A, B] zip lvar[C]
    @inline def lvar4[A, B, C, D]                = lvar3[A, B, C] zip lvar[D]
    @inline def lvar5[A, B, C, D, E]             = lvar4[A, B, C, D] zip lvar[E]
    @inline def lvar6[A, B, C, D, E, F]          = lvar5[A, B, C, D, E] zip lvar[F]
    @inline def lvar7[A, B, C, D, E, F, G]       = lvar6[A, B, C, D, E, F] zip lvar[G]
    @inline def lvar8[A, B, C, D, E, F, G, H]    = lvar7[A, B, C, D, E, F, G] zip lvar[H]
    @inline def lvar9[A, B, C, D, E, F, G, H, I] = lvar8[A, B, C, D, E, F, G, H] zip lvar[I]

    @inline def _fresh[V]                         = Fresh.fresh[V] _
    @inline def fresh1[A]                         = _fresh(lvar1[A])
    @inline def fresh2[A, B]                      = _fresh(lvar2[A, B])
    @inline def fresh3[A, B, C]                   = _fresh(lvar3[A, B, C])
    @inline def fresh4[A, B, C, D]                = _fresh(lvar4[A, B, C, D])
    @inline def fresh5[A, B, C, D, E]             = _fresh(lvar5[A, B, C, D, E])
    @inline def fresh6[A, B, C, D, E, F]          = _fresh(lvar6[A, B, C, D, E, F])
    @inline def fresh7[A, B, C, D, E, F, G]       = _fresh(lvar7[A, B, C, D, E, F, G])
    @inline def fresh8[A, B, C, D, E, F, G, H]    = _fresh(lvar8[A, B, C, D, E, F, G, H])
    @inline def fresh9[A, B, C, D, E, F, G, H, I] = _fresh(lvar9[A, B, C, D, E, F, G, H, I])

    @inline def _query[V]                               = Query.query[V] _
    @inline def query1[R, X, A]                         = _query(lvar1[A]).apply[R, X] _
    @inline def query2[R, X, A, B]                      = _query(lvar2[A, B]).apply[R, X] _
    @inline def query3[R, X, A, B, C]                   = _query(lvar3[A, B, C]).apply[R, X] _
    @inline def query4[R, X, A, B, C, D]                = _query(lvar4[A, B, C, D]).apply[R, X] _
    @inline def query5[R, X, A, B, C, D, E]             = _query(lvar5[A, B, C, D, E]).apply[R, X] _
    @inline def query6[R, X, A, B, C, D, E, F]          = _query(lvar6[A, B, C, D, E, F]).apply[R, X] _
    @inline def query7[R, X, A, B, C, D, E, F, G]       = _query(lvar7[A, B, C, D, E, F, G]).apply[R, X] _
    @inline def query8[R, X, A, B, C, D, E, F, G, H]    = _query(lvar8[A, B, C, D, E, F, G, H]).apply[R, X] _
    @inline def query9[R, X, A, B, C, D, E, F, G, H, I] = _query(lvar9[A, B, C, D, E, F, G, H, I]).apply[R, X] _

    import Unify.Unify1

    type Termo1[-R, +X, -A]                     = LTerm[A] => Goal[R, X]
    type Termo2[-R, +X, -A, -B]                 = (LTerm[A], LTerm[B]) => Goal[R, X]
    type Termo3[-R, +X, -A, -B, -C]             = (LTerm[A], LTerm[B], LTerm[C]) => Goal[R, X]
    type Termo4[-R, +X, -A, -B, -C, -D]         = (LTerm[A], LTerm[B], LTerm[C], LTerm[D]) => Goal[R, X]
    type Termo5[-R, +X, -A, -B, -C, -D, -E]     = (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E]) => Goal[R, X]
    type Termo6[-R, +X, -A, -B, -C, -D, -E, -F] =
      (LTerm[A], LTerm[B], LTerm[C], LTerm[D], LTerm[E], LTerm[F]) => Goal[R, X]

    import zkanren.reifyVar

    def termo1[R, X, A](f: LVar[A] => Goal[R, X])(implicit u: Unify1[R, X, LTerm[A]]): Termo1[R, X, A] =
      reifyVar[R, X, A](_)(f)

    def termo2[R, X, A, B](
      f: (LVar[A], LVar[B]) => Goal[R, X]
    ): Termo2[R, X, A, B] =
      (a, b) => reifyVar(a)(a => reifyVar(b)(b => f(a, b)))

    def termo3[R, X, A, B, C](
      f: (LVar[A], LVar[B], LVar[C]) => Goal[R, X]
    ): Termo3[R, X, A, B, C] =
      (a, b, c) => reifyVar(a)(a => reifyVar(b)(b => reifyVar(c)(c => f(a, b, c))))

    def termo4[R, X, A, B, C, D](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D])) => Goal[R, X]
    ): Termo4[R, X, A, B, C, D] =
      (a, b, c, d) => reifyVar(a)(a => reifyVar(b)(b => reifyVar(c)(c => reifyVar(d)(d => f(a, b, c, d)))))

    def termo5[R, X, A, B, C, D, E](
      f: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E])) => Goal[R, X]
    ): Termo5[R, X, A, B, C, D, E] =
      (a, b, c, d, e) =>
        reifyVar(a)(a => reifyVar(b)(b => reifyVar(c)(c => reifyVar(d)(d => reifyVar(e)(e => f(a, b, c, d, e))))))

    def termo6[R, X, A, B, C, D, E, F](
      m: ((LVar[A], LVar[B], LVar[C], LVar[D], LVar[E], LVar[F])) => Goal[R, X]
    ): Termo6[R, X, A, B, C, D, E, F] =
      (a, b, c, d, e, f) =>
        reifyVar(a)(a =>
          reifyVar(b)(b => reifyVar(c)(c => reifyVar(d)(d => reifyVar(e)(e => reifyVar(f)(f => m(a, b, c, d, e, f))))))
        )

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

  trait Relations {
    import zkanren._

    //    def conso[R, E, A](h: LTerm[A], t: LTerm[Iterable[LTerm[A]]], o: LTerm[Iterable[LTerm[A]]])(implicit
//      ua: Unify1[R, E, LTerm[A]]
//    ) = {
//      val x = Unify.terms[R, E, A, Iterable[LTerm[A]]] {
//        case (a, it) => ???
//      }
//
//      ???
//    }

  }

}
