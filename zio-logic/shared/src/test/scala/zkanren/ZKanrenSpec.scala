package zkanren

import zio._
import zio.test._

import scala.language.implicitConversions

object ZKanrenSpec extends ZIOSpecDefault {

  def spec = suite("ZKanren")(
    test("hello world") {
      import core._, Goal._
      val k = query[Any, Nothing, Int](x => equalTerm(x, Val(99)))
        .debug("QUERY")
      k.runHead.map {
        case Some(value: Val[Int]) => assertTrue(value() == 99)
        case _                     => assertNever("Should have resolved variable to 99")
      }
    }
  )

}
