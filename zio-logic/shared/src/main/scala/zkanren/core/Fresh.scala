package zkanren.core

import zio.stm.USTM
import zio.stream.ZStream

private[core] trait Fresh { self: Goal =>

  type T1[T[+_], A]          = T[A]
  type T2[T[+_], A, B]       = (T[A], T[B])
  type T3[T[+_], A, B, C]    = (T[A], T[B], T[C])
  type T4[T[+_], A, B, C, D] = (T[A], T[B], T[C], T[D])

  def fresh[R, E, A]: (T1[Var, A] => Goal[R, E]) => Goal[R, E] =
    freshN(_.fresh[A])

  def fresh2[R, E, A, B]: (T2[Var, A, B] => Goal[R, E]) => Goal[R, E] =
    freshN(s => s.fresh[A] <*> s.fresh[B])

  def fresh3[R, E, A, B, C]: (T3[Var, A, B, C] => Goal[R, E]) => Goal[R, E] =
    freshN { s =>
      s.fresh[A]
        .zip[Any, Nothing, Var[B]](s.fresh)
        .zip[Any, Nothing, Var[C]](s.fresh)
    }

  def fresh4[R, E, A, B, C, D]: (T4[Var, A, B, C, D] => Goal[R, E]) => Goal[R, E] =
    freshN { s =>
      s.fresh[A]
        .zip[Any, Nothing, Var[B]](s.fresh)
        .zip[Any, Nothing, Var[C]](s.fresh)
        .zip[Any, Nothing, Var[D]](s.fresh)
    }

  def freshN[R, E, T](k: State => USTM[T])(f: T => Goal[R, E]): Goal[R, E] = Goal { state =>
    ZStream.fromZIO(k(state).commit).flatMap(f(_)(ZStream.succeed(Right(state))))
  }

  def query[R, E, A]: (T1[Var, A] => Goal[R, E]) => ZStream[R, E, T1[Term, A]] =
    queryN[R, E, T1[Var, A], T1[Term, A]](_.fresh[A])(
      a => Seq(a),
      { case Seq(a) => a.asInstanceOf[Term[A]] }
    )

  def query2[R, E, A, B]: (T2[Var, A, B] => Goal[R, E]) => ZStream[R, E, T2[Term, A, B]] =
    queryN[R, E, T2[Var, A, B], T2[Term, A, B]](s => s.fresh[A].zip(s.fresh[B]))(
      { case (a, b) => Seq(a, b) },
      { case Seq(a, b) => (a, b).asInstanceOf[T2[Term, A, B]] }
    )

  def query3[R, E, A, B, C]: (T3[Var, A, B, C] => Goal[R, E]) => ZStream[R, E, T3[Term, A, B, C]] =
    queryN[R, E, T3[Var, A, B, C], T3[Term, A, B, C]](s =>
      s.fresh[A]
        .zip[Any, Nothing, Var[B]](s.fresh)
        .zip[Any, Nothing, Var[C]](s.fresh)
    )(
      { case (a, b, c) => Seq(a, b, c) },
      { case Seq(a, b, c) => (a, b, c).asInstanceOf[T3[Term, A, B, C]] }
    )

  def query4[R, E, A, B, C, D]: (T4[Var, A, B, C, D] => Goal[R, E]) => ZStream[R, E, T4[Term, A, B, C, D]] =
    queryN[R, E, T4[Var, A, B, C, D], T4[Term, A, B, C, D]](s =>
      s.fresh[A]
        .zip[Any, Nothing, Var[B]](s.fresh)
        .zip[Any, Nothing, Var[C]](s.fresh)
        .zip[Any, Nothing, Var[D]](s.fresh)
    )(
      { case (a, b, c, d) => Seq(a, b, c, d) },
      { case Seq(a, b, c, d) => (a, b, c, d).asInstanceOf[T4[Term, A, B, C, D]] }
    )

  def queryN[R, E, T, O](
    k: State => USTM[T]
  )(x: T => Seq[Var[_]], y: Seq[Term[_]] => O)(f: T => Goal[R, E]): ZStream[R, E, O] =
    ZStream.fromZIO(State.empty().flatMap(s => k(s).map(s -> _)).commit).flatMap { case (state, vars) =>
      f(vars)(ZStream.succeed(Right(state))).collectRight.mapZIO(_.query(x(vars)).map(y).commit)
    }

}
