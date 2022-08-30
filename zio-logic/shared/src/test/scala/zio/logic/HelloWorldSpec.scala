package zkanren

import zio._
import zio.stm.ZSTM
import zio.stream._
import zio.test._

import scala.annotation.tailrec
import scala.language.implicitConversions

// A microKanren implementation in ZIO2 ZStreams
object ZKanren {

  sealed trait Term[+A]
  case class Var[+A](intern: Int) extends Term[A]

  class Val[+A](private val thunk: () => A) extends Term[A] {
    def apply(): A = thunk()
  }
  object Val {
    def apply[A](a: => A): Val[A] = new Val(() => a)
  }

  case class State(
    private[ZKanren] val bindings: Map[Var[_], Term[_]],
    private[ZKanren] val nextVar: Int
  )

  object State {
    val empty: State = State(Map.empty, 0)

    def bind[A](x: Var[A], y: Term[A])(state: State): State =
      state.copy(bindings = state.bindings + (x -> y))

    def fresh[A](state: State): (Var[A], State) = {
      val x = Var[A](state.nextVar)
      (x, state.copy(nextVar = state.nextVar + 1))
    }

    @tailrec
    def walk[A](t: Term[A], seen: Seq[Term[A]])(state: State): (Term[A], Seq[Term[A]]) =
      t match {
        case x: Var[A] =>
          state.bindings.get(x) match {
            case Some(y: Var[A] @unchecked) => walk(y, y +: seen)(state)
            case _                          => (x, seen)
          }
        case _         => (t, seen)
      }

    // Right means the terms are bindable.
    // Left(Some) means the terms are equal (==) and no binding needs to be done.
    // Left(None) means the terms are not equal and cannot be bound.
    type Bindable[A] = Either[Option[Term[A]], (Var[A], Term[A])]
    def bindable[A](a: Term[A], b: Term[A])(state: State): Bindable[A] = {
      val (aVal, aSeen) = walk(a, Seq(a))(state)
      val (bVal, bSeen) = walk(b, Seq(b))(state)

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

  type Goal[R, E] = ZStream[R, E, Either[State, State]] => ZStream[R, E, Either[State, State]]
  object Goal {

    def apply[R, E](f: State => ZStream[R, E, Either[State, State]]): Goal[R, E] =
      _.flatMap {
        case Left(state)  => ZStream.succeed(Left(state))
        case Right(state) => f(state)
      }

    def fresh[R, E, A](f: Var[A] => Goal[R, E]): Goal[R, E] = Goal { state =>
      val (newVar, newState) = State.fresh[A](state)
      f(newVar)(ZStream.succeed(Right(newState)))
    }

    def accept[R, E]: Goal[R, E] = identity
    def reject[R, E]: Goal[R, E] = negation

    def negation[R, E]: Goal[R, E] = _.map(_.swap)

    def conjunction[R, E](goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { stream =>
      goals.foldLeft(goal(stream)) { case (prev, goal) =>
        prev.flatMapPar(n = 16) {
          case Left(state)  => ZStream.succeed(Left(state))
          case Right(state) => goal(ZStream.succeed(Right(state)))
        }
      }
    }

    def disjunction[R, E](goal: Goal[R, E], goals: Goal[R, E]*): Goal[R, E] = { stream =>
      val streams = (goal +: goals).map(_(stream))
      ZStream.mergeAll(n = 16)(streams: _*)
    }

    def equalTerm[R, E, A](a: Term[A], b: Term[A]): Goal[R, E] = Goal { state =>
      State.bindable(a, b)(state) match {
        case Left(Some(_)) => ZStream.succeed(Right(state))
        case Left(None)    => ZStream.succeed(Left(state))
        case Right((x, y)) => ZStream.succeed(Right(State.bind(x, y)(state)))
      }
    }

    def equal[R, E, A](a: A, b: A)(implicit unify: Unify[A]): Goal[R, E] = unify(a, b)

    def equalProduct[R, E, A](a: Iterable[A], b: Iterable[A])(implicit unify: Unify[A]): Goal[R, E] = {
      val sameLength: Goal[R, E]         = equalTerm(Val(a.size), Val(b.size))
      val equalElements: Seq[Goal[R, E]] = a.zip(b).map { case (a, b) => unify[R, E](a, b) }.toSeq
      conjunction(sameLength, equalElements: _*)
    }

    def equalCommit[R, E, A](a: ZSTM[R, E, A], b: ZSTM[R, E, A])(implicit unify: Unify[A]): Goal[R, E] = Goal { state =>
      ZStream.fromZIO(a.zip(b).commit.either).flatMap {
        case Left(e)       => ZStream.succeed(Left(state))
        case Right((a, b)) => unify[R, E](a, b)(ZStream.succeed(Right(state)))
      }
    }

    def equalStream[R, E, A](a: ZStream[R, E, A], b: ZStream[R, E, A])(implicit unify: Unify[A]): Goal[R, E] = Goal {
      state =>
        a.zipWith(b) { case (a, b) => unify[R, E](a, b) }.either.flatMap {
          case Left(e)     => ZStream.succeed(Left(state))
          case Right(goal) => goal(ZStream.succeed(Right(state)))
        }
    }

    def equalZIO[R, E, A](a: ZIO[R, E, A], b: ZIO[R, E, A])(implicit unify: Unify[A]): Goal[R, E] =
      equalStream(ZStream.fromZIO(a), ZStream.fromZIO(b))

    def run[R, E](goal: Goal[R, E]): ZStream[R, E, State] =
      goal(ZStream.succeed(Right(State.empty))).collectRight

  }

  trait Unify[-A] {
    def apply[R, E](a: => A, b: => A): Goal[R, E]
  }

  object Unify {
    implicit def anyValTerm[A <: AnyVal](a: A): Term[A] = Val(a)

    def terms[A]: Unify[Term[A]] = new Unify[Term[A]] {
      override def apply[R, E](a: => Term[A], b: => Term[A]): Goal[R, E] = Goal.equalTerm[R, E, A](a, b)
    }

  }

}

object HelloWorldSpec extends ZIOSpecDefault {

  def spec = suite("HelloWorldSpec")(
    test("hello world") {
      for {
        _ <- Console.printLine("Hello, World!").orDie
      } yield assertTrue(true)
    }
  )

}
