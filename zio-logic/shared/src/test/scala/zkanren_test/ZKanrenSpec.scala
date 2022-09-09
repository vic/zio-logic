package zkanren_test

import zio.{Chunk, Trace, ZIO}
import zio.stream.ZStream
import zio.test.Spec.TestCase
import zio.test.TestAspect.{ignore, sequential, timed}
import zio.test._
import zkanren._

import scala.reflect.{ClassTag, classTag}

object ZKanrenSpec extends ZIOSpecDefault {

  def testRunEmpty(program: ZStream[State, Nothing, Any]): ZIO[State, Nothing, TestResult] =
    program.runHead.map(head => assertTrue(head.isEmpty))

  def testRunSingle[T: ClassTag](
    program: ZStream[State, Nothing, Any]
  )(test: T => TestResult): ZIO[State, Nothing, TestResult] =
    program.runHead.map {
      case Some(t: T) if classTag[T].runtimeClass.isInstance(t) =>
        test(t)

      case x =>
        assert(x)(Assertion.assertion("Unexpected first result")(_ => false))
    }

  private val testEmptyUnification =
    test("empty unification") {
      val program = query1[Int](a => lval(99) =:= lval(22))
      testRunEmpty(program)
    }

  private val testUnificationOfVariableToItself =
    test("unification of forall values in variable") {
      val program = query1[Int](a => lval(99) =:= lval(99))
      testRunSingle[LVar[Int]](program)(v => assertTrue(v.variable == 0L))
    }

  private val testUnificationOfVariableToValue =
    test("unification binds variables assigned to other variables until a val is found.") {
      val program = query1[Int] { a =>
        fresh3[Int, Int, Int] { case (x, y, z) => a =:= z && x =:= lval(99) && z =:= y && x =:= y }
      }
      testRunSingle[LVal[Int]](program)(a => assertTrue(a.value == 99))
    }

  private val testUnificationOfIterablesOfSameLength =
    test("unification over iterables of same length") {
      val program = query3[Int, Int, Int] { case (a, b, c) =>
        val seq1 = Seq(a, lval(2), c)
        val seq2 = Seq(lval(1), b, lval(3))
        seq1 =:= seq2
      }
      testRunSingle[(LVal[Int], LVal[Int], LVal[Int])](program) { case (a, b, c) =>
        assertTrue((a.value, b.value, c.value) == (1, 2, 3))
      }
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

      val program = query2[String, Int] { case (name, age) =>
        val a: Person[LTerm] = Person(name = name, age = lval(22))
        val b: Person[LTerm] = Person(name = lval("Melo"), age = age)
        a =:= b
      }

      testRunSingle[(LVal[String], LVal[Int])](program) { case (a, b) =>
        assertTrue((a.value, b.value) == ("Melo", 22))
      }
    }

  override def spec = suite("ZKanren")(
    suite("Unification")(
      testEmptyUnification,
      testUnificationOfVariableToItself,
      testUnificationOfVariableToValue,
      testUnificationOfIterablesOfSameLength,
      testUnificationOverCustomProduct
    )
  ).provideCustomLayer(emptyStateLayer) @@ timed

}
