package bep.free

import bep.core.{Expr, Pattern, Value}

import language.higherKinds
import scala.collection.immutable.Stream.Empty

abstract class Free[A] {
  def bind(k: A => Free[A]): Free[A]

  def values(): Stream[Free[A]]

  def get(): A
}

object Free {
  def step[A](free: Stream[Free[A]]): Stream[Free[A]] = {
    free match {
      case Pure(_) #:: xs => xs
      case (f @ Fork(_, _, _)) #:: xs => xs #::: f.asInstanceOf[Fork[A]].values()
      case Empty => Empty
      case x => throw new IllegalArgumentException(s"Could not parse $x")
    }
  }

  def bfs[A](free: Stream[Free[A]]): A = free match {
    case Empty => Nil.asInstanceOf[A]
    case Pure(x) #:: _ => x
    case _ => bfs(step(free))
  }
}

case class Pure[A](x: A) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = k(x)

  override def values(): Stream[Free[A]] = Stream(Pure(x))

  override def get(): A = x
}

case class Fork[A](v: A, patterns: List[(Pattern, Expr)], f: (A, List[(Pattern, Expr)]) => Stream[Free[A]]) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = Flow(this.values()).bind(k)

  override def values(): Stream[Free[A]] = f(v, patterns)

  override def get(): A = Free.bfs(this.values())
}

case class Flow[A](x: Stream[Free[A]]) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = Flow(x.map(f => k(f.get())))

  override def values(): Stream[Free[A]] = x

  override def get(): A = x.head.get()
}
