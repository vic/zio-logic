package zkanren.test

import zio.stream.ZStream
import zio.test.TestAspect.ignore
import zio.test._
import zkanren._

object ZKanrenSpec extends ZIOSpecDefault {

  def spec = suite("ZKanren")(
    test("unification binds variables assigned to other variables until a val is found.") {
      val program: ZStream[State, Nothing, LTerm[Int]] =
        query(lvar[Int]) { a =>
          fresh(lvar3[Int, Int, Int]) { case (x, y, z) =>
            a =:= x && y =:= lval(99) && z =:= x && y =:= z
          }
        }
      program.runHead.map {
        case Some(value: LVal[Int]) => assertTrue(value() == 99)
        case x                      => assertNever(s"Should have resolved variable to 99. ${x}")
      }
    },
    test("unification over iterables of same length") {
      val program = query(lvar3[Int, Int, Int]) { case (a, b, c) =>
        val seq1 = Seq(a, lval(2), c)
        val seq2 = Seq(lval(1), b, lval(3))
        seq1 =:= seq2
      }

      program.runHead.map {
        case Some((a: LVal[Int], b: LVal[Int], c: LVal[Int])) => assertTrue((a(), b(), c()) == (1, 2, 3))
        case x                                                => assertNever(s"Should have resolved sequence ${x}")
      }
    }
  ).provideCustomLayer(emptyStateLayer)

}
