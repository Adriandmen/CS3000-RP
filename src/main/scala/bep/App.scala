package bep

import bep.core._
import bep.interp.Interpreter
import bep.interp.Interpreter.Result
import bep.syntax.Syntax._

/**
  * @author Adrian Mensing
  */
object App {

  private def length = letrec("length")
  private def f = letrec("f")
  private def amb = letrec("amb")
  private def coin = letrec("coin")

  private def L = Var("L")
  private def a = Var("a")
  private def b = Var("b")
  private def n = Var("n")
  private def m = Var("m")
  private def x = Var("x")
  private def xs = Var("xs")
  private val Empty = Val("Empty", Nil)
  private val Cons: (Any, Expr) => Val = (e1, e2) => e1 match {
    case Var(_) => Val("Cons", List(e1.asInstanceOf[Var], e2))
    case Val(_, _) => Val("Cons", List(e1.asInstanceOf[Val], e2))
    case _ => Val("Cons", List(Val(e1, Nil), e2))
  }

  def main(args : Array[String]): Unit = {
    val code = Interpreter.concat(List(
//      amb(a, b) :- matching(Var("dummy"))
//        -> (Pattern(Val("dummy1", Nil)), a)
//        -> (Pattern(Val("dummy2", Nil)), b),
//
//      coin() :- amb(Num(0), Num(1)),
//
//      coin()
      length(L) :-
        matching(L)
          -> (Pattern(Empty), Num(0))
          -> (Pattern(Cons(x, xs)), Plus(Num(1), length(xs))),

      Exists(Var("l"), Equals(length(Var("l")), Num(2)))
    ))

    println(code)

    return

    val result = Interpreter.interp(code)

//    println(code)
    println(result.bfs().map(r => formalized(r)).toSet)

//    result.bfs().head.foreach(p => println(formalized(p)))
  }

  def formalized(result: Result): String = result._1 match {
    case ValV(n, Nil) => n.toString
    case x => x.toString
  }
}


//      length(L) :- matching(L)
//        -> (Pattern(Empty), Num(0))
//        -> (Pattern(Cons(x, xs)), Plus(Num(1), length(xs))),

//      Exists(n, Equals(length(n), Num(1)))
//      Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
