package zkanren_test

import zio.Chunk
import zio.stream.ZStream
import zio.test.TestAspect.{ignore, timed}
import zio.test._
import zkanren._
import Assertion._

object ZKanrenSpec extends ZIOSpecDefault {

  def spec = suite("ZKanren")(
    test("Term unification") {
      val program = query1[Int] { a =>
        fresh1[Int] { x =>
          a =:= lval(42)
        }
      }
      program.runHead.map {
        case Some(a: LVal[Int]) => assertTrue(a() == 42)
        case x                  => assert(x)(assertion("Should have been assigned 42")(_ => false))
      }
    }
//    suite("Unification")(
//      test("unification binds variables assigned to other variables until a val is found.") {
//        val program: ZStream[State, Nothing, LTerm[Int]] =
//          query1[Int] { a =>
//            fresh3[Int, Int, Int] { case (x, y, z) =>
//              conj(
//                a =:= y,
//                y =:= lval(99)
//              )
//            }
//          }
//        program.runHead.map {
//          case Some(value: LVal[Int]) => assertTrue(value() == 99)
//          case x                      => assertNever(s"Should have resolved variable to 99. ${x}")
//        }
//      },
//      test("unification") {
//        val program =
//          query2[Int, Int] { case (a, b) =>
//            conj(a =:= b, b =:= lval(3))
//          }
//        program.runHead.map {
//          case Some((a: LVal[Int], b: LVal[Int])) => assertTrue((a(), b()) == (3, 3))
//          case x                                  => assertNever(s"Should have resolved variable to 99. ${x}")
//        }
//      },
//      test("unification over iterables of same length") {
//        val program = query3[Int, Int, Int] { case (a, b, c) =>
//          val seq1 = Seq(a, lval(2), c)
//          val seq2 = Seq(lval(1), b, lval(3))
//          seq1 =:= seq2
//        }
//
//        program.runHead.map {
//          case Some((a: LVal[Int], b: LVal[Int], c: LVal[Int])) => assertTrue((a(), b(), c()) == (1, 2, 3))
//          case x                                                => assertNever(s"Should have resolved sequence ${x}")
//        }
//      },
//      test("unification over custom product") {
//
//        case class Person[T[+_]](
//          name: T[String],
//          age: T[Int]
//        )
//
//        implicit val unifyPerson: Unify1[Any, Nothing, Person[LTerm]] = Unify.one[Person[LTerm]] { case (a, b) =>
//          a.name =:= b.name && a.age =:= b.age
//        }
//
//        val program = query2[String, Int] { case (name, age) =>
//          val a: Person[LTerm] = Person(name = name, age = lval(22))
//          val b: Person[LTerm] = Person(name = lval("Melo"), age = age)
//          a =:= b
//        }
//
//        program.runHead.map {
//          case Some((a: LVal[String], b: LVal[Int])) => assertTrue((a(), b()) == ("Melo", 22))
//          case x                                     => assertNever(s"Should have resolved product ${x}")
//        }
//      }
//    )
  ).provideCustomLayer(emptyStateLayer) @@ timed

}
