package zkanren.test

import zio.stream.ZStream
import zio.test.TestAspect.ignore
import zio.test._
import zkanren._

import scala.language.implicitConversions

object ZKanrenSpec extends ZIOSpecDefault {

  def spec = suite("ZKanren")(
    test("should unify simple int terms") {
      val program: ZStream[State, Nothing, LTerm[Int]] =
        query(lvar[Int]) { a =>
          fresh(lvar3[Int, Int, Int]) { case (x, y, z) =>
            a =:= x && y =:= lval(99) && z =:= x && y =:= z
          }
        }
      program.runHead.map {
        case Some(value: LVal[Int]) => assertTrue(value() == 99)
        case _                      => assertNever("Should have resolved variable to 99")
      }
    },
    test("other") {
      val x = query(lvar3[Int, Int, Int]) { case (a, b, c) =>
        ???
      }
      assertTrue(false)
    } @@ ignore
  ).provideCustomLayer(emptyStateLayer)

}
