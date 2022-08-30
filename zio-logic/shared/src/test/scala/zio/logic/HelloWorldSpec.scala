package zkanren

import zio._
import zio.stm.ZSTM
import zio.stream._
import zio.test._

import scala.annotation.tailrec

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

  type Goal[R] = ZStream[R, State, State] => ZStream[R, State, State]
  object Goal {

    def fresh[R, A](f: Var[A] => Goal[R]): Goal[R] = { stream =>
      stream.flatMap { state =>
        val (newVar, newState) = State.fresh[A](state)
        f(newVar)(ZStream.succeed(newState))
      }
    }

    def accept[R]: Goal[R] = identity
    def reject[R]: Goal[R] = negation

    def negation[R]: Goal[R] = { stream =>
      stream.either.map(_.swap).absolve
    }

    def conjunction[R](goal: Goal[R], goals: Goal[R]*): Goal[R] = { stream =>
      goals.foldLeft(goal(stream)) { case (prev, goal) => prev.flatMapPar(n = 16)(s => goal(ZStream.succeed(s))) }
    }

    def disjunction[R](goal: Goal[R], goals: Goal[R]*): Goal[R] = { stream =>
      val streams = (goal +: goals).map(_(stream))
      ZStream.mergeAll(n = 16)(streams: _*)
    }

    def equalTerm[R, A](a: Term[A], b: Term[A]): Goal[R] = { stream =>
      stream.flatMap { state =>
        State.bindable(a, b)(state) match {
          case Left(Some(_)) => ZStream.succeed(state)
          case Left(None)    => ZStream.fail(state)
          case Right((x, y)) => ZStream.succeed(State.bind(x, y)(state))
        }
      }
    }

    def equal[R, A](a: A, b: A)(implicit unify: Unify[A]): Goal[R] = unify(a, b)

    def equalProduct[R, A](a: Iterable[A], b: Iterable[A])(implicit unify: Unify[A]): Goal[R] = {
      val sameLength: Goal[R]         = equalTerm(Val(a.size), Val(b.size))
      val equalElements: Seq[Goal[R]] = a.zip(b).map { case (a, b) => unify[R](a, b) }.toSeq
      conjunction(sameLength, equalElements: _*)
    }

    def equalCommit[R, A](a: ZSTM[R, Any, A], b: ZSTM[R, Any, A])(implicit unify: Unify[A]): Goal[R] = { stream =>
      ZStream.fromZIO(a.zip(b).commit.either).flatMap {
        case Left(_)       => reject[R](stream)
        case Right((a, b)) => unify[R](a, b)(stream)
      }
    }

    def equalStream[R, A](a: ZStream[R, Any, A], b: ZStream[R, Any, A])(implicit unify: Unify[A]): Goal[R] = { stream =>
      a.zipWith(b) { case (a, b) => unify[R](a, b) }.either.flatMap {
        case Left(_)     => reject[R](stream)
        case Right(goal) => goal(stream)
      }
    }

    def equalZIO[R, A](a: ZIO[R, Any, A], b: ZIO[R, Any, A])(implicit unify: Unify[A]): Goal[R] =
      equalStream(ZStream.fromZIO(a), ZStream.fromZIO(b))

  }

  trait Unify[-A] {
    def apply[R](a: A, b: A): Goal[R]
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
