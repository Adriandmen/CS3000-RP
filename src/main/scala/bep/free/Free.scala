package bep.free

import bep.`match`.Matcher
import bep.core.{Expr, Pattern, Value}
import bep.interp.Interpreter.Result
import bep.interp.{Environment, Interpreter}

import language.higherKinds

abstract class Free[A] {
  def bind(k: A => Free[A]): Free[A]

  def values(): Stream[Free[A]]

  def get(): A
}

case class Pure[A](x: A) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = k(x)

  override def values(): Stream[Free[A]] = Stream(Pure(x))

  override def get(): A = x
}

case class Fork[A](v: A, patterns: List[(Pattern, Expr)], f: (A, List[(Pattern, Expr)]) => Stream[Free[A]]) extends Free[A] {
  type Result = (Value, Environment)

  override def bind(k: A => Free[A]): Free[A] = k(this.get())

  def values(): Stream[Free[A]] = {
    f(v, patterns)
  }

  override def get(): A = {
    this.values().head.get()
  }
}
