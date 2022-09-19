package zkanren_test

import zio.ZIO
import zio.stream.ZStream
import zio.test.Assertion.assertion
import zio.test.TestAspect.timed
import zio.test.{TestResult, ZIOSpecDefault, assert, assertTrue}
import zkanren._

object ZKanrenSpec extends ZIOSpecDefault {

  def testRunEmpty(program: ZStream[State, Nothing, Any]): ZIO[State, Nothing, TestResult] =
    program.runHead.map(head => assertTrue(head.isEmpty))

  def testRunSingle[T](
    program: ZStream[State, Nothing, Any]
  )(test: T => TestResult): ZIO[State, Nothing, TestResult] =
    program.runHead.map {
      case Some(t: T) =>
        test(t)

      case x =>
        assert(x)(assertion("Unexpected first result")(_ => false))
    }

  private val testEmptyUnification =
    test("empty unification") {
      val program = query1[Any, Nothing, Int].apply(a => 99 =:= 22)
      testRunEmpty(program)
    }

  private val testUnificationOfVariableToItself =
    suite("unification of same value") {
      def assertUnifyToSelf(
        goal: => Goal[Any, Nothing]
      ): ZIO[zkanren.State, Nothing, TestResult] = {
        val program = query1[Any, Nothing, Int](_ => goal)
        testRunSingle[LVar[Int]](program)(v => assertTrue(v.variable == 0L))
      }

      test("both literals")(assertUnifyToSelf(99 =:= 99)) +
        test("left lval")(assertUnifyToSelf(lval(99) =:= 99)) +
        test("right lval")(assertUnifyToSelf(99 =:= lval(99))) +
        test("both lval")(assertUnifyToSelf(lval(99) =:= lval(99)))
    }

  private val testUnificationOfVariableToValue =
    test("unification binds variables assigned to other variables until a val is found.") {
      val program = query1[Any, Nothing, Int] { a =>
        fresh3[Int, Int, Int] { case (x, y, z) =>
          a =:= z && x =:= 99 && z =:= y && x =:= y
        }
      }
      testRunSingle[LVal[Int]](program)(a => assertTrue(a.value == 99))
    }

  private val testUnificationOverIdentity =
    test("unification over identity") {
      final case object Singleton
      implicit val idUnification: Unify[Any, Nothing, Singleton.type, Singleton.type] = Unify.identity[Singleton.type]

      val program = query1[Any, Nothing, Singleton.type](a => Singleton =:= a)
      testRunSingle[LVal[Singleton.type]](program)(a => assertTrue(a.value == Singleton))
    }

  private val testUnificationOverTuple =
    test("unification over tuple2") {
      val program = query1[Any, Nothing, Int] { a =>
        (a, lval(33)) =:= (lval(33), a)
      }
      testRunSingle[LVal[Int]](program)(a => assertTrue(a.value == 33))
    }

  private val testUnificationOfIterablesOfSameLength =
    test("unification over iterables of same length") {
      def assertUnifyIterables(
        f: ((Seq[LTerm[Int]], Seq[LTerm[Int]])) => Goal[Any, Nothing]
      ): ZIO[zkanren.State, Nothing, TestResult] = {
        val program = query3[Any, Nothing, Int, Int, Int] { case (a, b, c) =>
          val seq1 = Seq(a, lval(2), c)
          val seq2 = Seq(lval(1), b, lval(3))
          f(seq1, seq2)
        }
        testRunSingle[(LVal[Int], LVal[Int], LVal[Int])](program) { case (a, b, c) =>
          assertTrue((a.value, b.value, c.value) == (1, 2, 3))
        }
      }

      assertUnifyIterables { case (as, bs) => as =:= bs }
    }

  private val testUnificationOverCustomProduct =
    suite("unification over custom product") {

      case class Person[T[+_]](
        name: T[String],
        age: T[Int]
      )

      implicit val unifyPerson: Unify1[Any, Nothing, Person[LTerm]] = Unify.one[Person[LTerm]] { case (a, b) =>
        a.name =:= b.name && a.age =:= b.age
      }

      def check(f: ((Person[LTerm], Person[LTerm]) => Goal[Any, Nothing])): ZIO[zkanren.State, Nothing, TestResult] = {
        val program = query2[Any, Nothing, String, Int] { case (name, age) =>
          val a: Person[LTerm] = Person(name = name, age = lval(22))
          val b: Person[LTerm] = Person(name = lval("Melo"), age = age)
          f(a, b)
        }

        testRunSingle[(LVal[String], LVal[Int])](program) { case (a, b) =>
          assertTrue((a.value, b.value) == ("Melo", 22))
        }
      }

      test("both values")(check { case (a, b) => a =:= b }) +
        test("both terms")(check { case (a, b) => lval(a) =:= lval(b) })
    }

  private val testUnificationOverOption = suite("unification over option") {
    test("Some and None do not unify") {
      val program = query1[Any, Nothing, Int] { v =>
        val a = Some(v)
        val b = None
        a =:= b
      }
      testRunEmpty(program)
    } +
      test("Some(lvar) and Some(lval) do unify") {
        val program = query1[Any, Nothing, Int] { v =>
          val a = Some(v)
          val b = Some(lval(99))
          a =:= b
        }
        testRunSingle[LVal[Int]](program)(v => assertTrue(v.value == 99))
      }
  }

//  private val testDefineATermFunction = test("termo creates a term unifying function") {
//    val x       = termo1[Any, Nothing, Int](_ =:= lval(5))
//    val program = query1[Int](x)
//    testRunSingle[LVal[Int]](program)(n => assertTrue(n.value == 5))
//  }

  override def spec = suite("ZKanren")(
    suite("Unification")(
      testEmptyUnification,
      testUnificationOfVariableToItself,
      testUnificationOfVariableToValue,
      testUnificationOverIdentity,
      testUnificationOverTuple,
      testUnificationOfIterablesOfSameLength,
      testUnificationOverCustomProduct,
      testUnificationOverOption
//      testDefineATermFunction
    )
  ).provideCustomLayer(emptyStateLayer) @@ timed

}
