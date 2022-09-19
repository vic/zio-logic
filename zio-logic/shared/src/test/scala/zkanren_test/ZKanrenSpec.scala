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

  private val testUnificationOfIterablesOfSameLength =
    suite("unification over iterables of same length") {
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

      test("both scala vals")(assertUnifyIterables { case (as, bs) => as =:= bs }) +
        test("both lvals")(assertUnifyIterables { case (as, bs) => lval(as) =:= lval(bs) })
    }

  private val testUnificationOverCustomProduct =
    test("unification over custom product") {

      case class Person[T[+_]](
        name: T[String],
        age: T[Int]
      )

      implicit val unifyPerson: Unify1[Any, Nothing, Person[LTerm]] = Unify.one[Person[LTerm]] { case (a, b) =>
        a.name =:= b.name && a.age =:= b.age
      }

      val program = query2[Any, Nothing, String, Int] { case (name, age) =>
        val a: Person[LTerm] = Person(name = name, age = lval(22))
        val b: Person[LTerm] = Person(name = lval("Melo"), age = age)
        a =:= b
      }

      testRunSingle[(LVal[String], LVal[Int])](program) { case (a, b) =>
        assertTrue((a.value, b.value) == ("Melo", 22))
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
      testUnificationOfIterablesOfSameLength,
      testUnificationOverCustomProduct
//      testDefineATermFunction
    )
  ).provideCustomLayer(emptyStateLayer) @@ timed

}
