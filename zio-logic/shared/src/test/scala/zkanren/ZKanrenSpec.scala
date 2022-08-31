package zkanren

import zio.test._
import zkanren.core.State

import scala.language.implicitConversions

object ZKanrenSpec extends ZIOSpecDefault {

  def spec = suite("ZKanren")(
    test("hello world") {
      import core._
      import Goal._
      import Unify._
      val k = query1[Any, Nothing, Int](q => unify(q, lval(99)))
      k.runHead.map {
        case Some(value: LVal[Int]) => assertTrue(value() == 99)
        case _                      => assertNever("Should have resolved variable to 99")
      }
    }.provideSomeLayer(State.empty)
  )

}
