package zkanren

import zio._
import zio.test._

import scala.language.implicitConversions

object HelloWorldSpec extends ZIOSpecDefault {

  def spec = suite("HelloWorldSpec")(
    test("hello world") {
      for {
        _ <- Console.printLine("Hello, World!").orDie
      } yield assertTrue(true)
    }
  )

}
