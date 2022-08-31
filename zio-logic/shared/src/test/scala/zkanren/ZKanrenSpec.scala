package zkanren

import zio.stream.ZStream
import zio.test._
import zkanren.api._

import scala.language.implicitConversions

object ZKanrenSpec extends ZIOSpecDefault {

  def spec = suite("ZKanren")(
    test("hello world") {
      val program: ZStream[State, Nothing, LTerm[Int]] = query(lvar[Int]) { a =>
        fresh(lvar[Int] <*> lvar[Int]) { case (x, y) =>
          a =:= x && y =:= lval(99) && y =:= x
        }
      }
      program.runHead.map {
        case Some(value: LVal[Int]) => assertTrue(value() == 99)
        case _                      => assertNever("Should have resolved variable to 99")
      }
    }.provideSomeLayer(emptyStateLayer)
  )

}
