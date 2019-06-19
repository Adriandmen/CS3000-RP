package bep.free

import bep.core.{Expr, Value}
import bep.free.Free.Bind

import scala.language.higherKinds
import scala.language.implicitConversions

sealed trait Free[F[_], A] {
  def bind[B](f: A => Free[F, B]): Free[F, B] = Bind(this, f)
}

object Free {
  type Id[A] = A

  // Mapping trait. Used for implementations of interpreters / executors.
  trait ~>[F[_], G[_]] {
    def interp[A](fa: F[A]): Free[F, A]
    def handle[A](free: Free[F, A]): G[A]
  }

  implicit def value2free[A](value: Value): Free[Expr, A] = Pure(value.asInstanceOf[A])

  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Bind[F[_], X, A](x: Free[F, X], f: X => Free[F, A]) extends Free[F, A]
  case class Fork[F[_], A](fa: List[F[A]], f: F[A] => Free[F, A]) extends Free[F, A]
}
