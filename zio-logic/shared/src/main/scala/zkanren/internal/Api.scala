package zkanren.internal

import zio.stm.ZSTM
import zio.stream.ZChannel
import zio.{Duration, ULayer, ZIO, ZLayer}
import zkanren.internal
import zkanren.internal.Goal.Chan

private[zkanren] trait Api extends Api.Exports with Api.FreshQuery with Api.Micro
private[zkanren] object Api {
  implicit def swapUnify[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, B, A] = { case (b, a) => u(a, b) }

  implicit def eventualUnifier[R, E, A, B](implicit u: Unify[R, E, A, B]): Unify[R, E, LTerm[A], LTerm[B]] = {
    case (a, b) =>
      zkanren.eventually.unify(a, b)
  }

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

    type Goal[-R, +E] = internal.Goal[R, E]
    lazy val Goal = internal.Goal

    lazy val Unify = internal.Unify

    lazy val emptyStateLayer: ULayer[State]                = State.empty
    @inline implicit def unifyOps[A]: A => Api.UnifyOps[A] = Api.UnifyOps[A] _
    @inline implicit def goalOps[R, E]                     = Api.GoalOps[R, E] _

    @inline implicit def unifyTerms[A]: Unify[Any, Nothing, LTerm[A], LTerm[A]] = Unify.terms[A]

    @inline implicit def unifyIterables[R, E, A, B](implicit
      u: Unify[R, E, A, B]
    ): Unify[R, E, IterableOnce[A], IterableOnce[B]] =
      Unify.iterables[R, E, A, B]
  }

  trait FreshQuery {
    @inline def lval[A] = LVal[A] _

    @inline def lvar[A]                          = Fresh.lvar[A]
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

    @inline def query[V]                          = Query.query[V] _
    @inline def query1[A]                         = query(lvar[A])
    @inline def query2[A, B]                      = query(lvar2[A, B])
    @inline def query3[A, B, C]                   = query(lvar3[A, B, C])
    @inline def query4[A, B, C, D]                = query(lvar4[A, B, C, D])
    @inline def query5[A, B, C, D, E]             = query(lvar5[A, B, C, D, E])
    @inline def query6[A, B, C, D, E, F]          = query(lvar6[A, B, C, D, E, F])
    @inline def query7[A, B, C, D, E, F, G]       = query(lvar7[A, B, C, D, E, F, G])
    @inline def query8[A, B, C, D, E, F, G, H]    = query(lvar8[A, B, C, D, E, F, G, H])
    @inline def query9[A, B, C, D, E, F, G, H, I] = query(lvar9[A, B, C, D, E, F, G, H, I])

    def term1[R, E, A](f: LVar[A] => Goal[R, E])(a: LTerm[A]): Goal[R, E]                            =
      fresh1[A](vA => vA =:= a && f(vA))
    def term2[R, E, A, B](f: (LVar[A], LVar[B]) => Goal[R, E])(a: LTerm[A], b: LTerm[B]): Goal[R, E] =
      fresh2[A, B] { case (vA, vB) => vA =:= a && vB =:= b && f(vA, vB) }
    def term3[R, E, A, B, C](
      f: (LVar[A], LVar[B], LVar[C]) => Goal[R, E]
    )(a: LTerm[A], b: LTerm[B], c: LTerm[C]): Goal[R, E]                                             =
      fresh3[A, B, C] { case (vA, vB, vC) => vA =:= a && vB =:= b && vC =:= c && f(vA, vB, vC) }
    def term4[R, E, A, B, C, D](
      f: (LVar[A], LVar[B], LVar[C], LVar[D]) => Goal[R, E]
    )(a: LTerm[A], b: LTerm[B], c: LTerm[C], d: LTerm[D]): Goal[R, E]                                =
      fresh4[A, B, C, D] { case (vA, vB, vC, vD) => vA =:= a && vB =:= b && vC =:= c && vD =:= d && f(vA, vB, vC, vD) }
    def term5[R, X, A, B, C, D, E](
      f: (LVar[A], LVar[B], LVar[C], LVar[D], LVar[E]) => Goal[R, X]
    )(a: LTerm[A], b: LTerm[B], c: LTerm[C], d: LTerm[D], e: LTerm[E]): Goal[R, X]                   =
      fresh5[A, B, C, D, E] { case (vA, vB, vC, vD, vE) =>
        vA =:= a && vB =:= b && vC =:= c && vD =:= d && vE =:= e && f(vA, vB, vC, vD, vE)
      }

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

    def unify[R, E, A, B](a: LTerm[A], b: LTerm[B], timeout: Duration = Duration.Infinity)(implicit
      u: Unify[R, E, A, B]
    ): Goal[R, E] = {
      import zkanren.{fresh2, all}
      fresh2[A, B] { case (vA, vB) =>
        def eff(s: State): ZIO[R, Nothing, Chan[R, E]] = State
          .reifyEventually(vA)
          .zip(State.reifyEventually(vB))
          .map { case (a, b) => u(a, b).toChannel }
          .commit
          .timeout(timeout)
          .map(_.getOrElse(ZChannel.write(Left(s))))
          .provideSomeLayer[R](ZLayer.succeed(s))

        all(
          a =:= vA,
          b =:= vB,
          Goal.fromReadLoop(s => ZChannel.unwrap(eff(s)))
        )
      }
    }
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

  implicit class LOptionOps[A](private val self: LTerm[Option[A]]) extends AnyVal {
    import zkanren._
    def isEmpty: Goal[Any, Nothing] = self =:= lval(None)

    def contains[R, E, B](item: LTerm[B])(implicit u: Unify[R, E, A, B]): Goal[Any, Nothing] =
      ???
  }

  trait Relations {
    import zkanren._

  }

}
