package bep.free

import bep.core.{Expr, Pattern, Value}

import language.higherKinds
import scala.collection.immutable.Stream.Empty

trait Free[A] {
  def bind(k: A => Free[A]): Free[A]

  def values(): Stream[Free[A]]

  def get(): A

  def bfs(): Stream[A]
}

object Free {
  def bfs[A](free: Stream[Free[A]]): Stream[A] = {
    free match {
      case Empty => Empty
      case Pure(x) #:: xs => x #:: bfs(xs)
      case (f @ Fork(_, _, _)) #:: xs => bfs(xs #::: f.values())
      case Flow(x) #:: xs => bfs(xs #::: x)
      case x => throw new IllegalArgumentException(s"Could not parse $x")
    }
  }
}

case class Pure[A](x: A) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = k(x)

  override def values(): Stream[Free[A]] = Stream(Pure(x))

  override def get(): A = x

  override def bfs(): Stream[A] = Free.bfs(this.values())
}

case class Fork[A](v: A, patterns: List[(Pattern, Expr)], f: (A, List[(Pattern, Expr)]) => Stream[Free[A]]) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = Flow(this.values()).bind(k)

  override def values(): Stream[Free[A]] = f(v, patterns)

  override def get(): A = this.bfs().head

  override def bfs(): Stream[A] = Free.bfs(this.values())
}

case class Flow[A](x: Stream[Free[A]]) extends Free[A] {
  override def bind(k: A => Free[A]): Free[A] = Flow(x.map(f => f.bind(k)))

  override def values(): Stream[Free[A]] = x

  override def get(): A = this.bfs().head

  override def bfs(): Stream[A] = Free.bfs(x)
}