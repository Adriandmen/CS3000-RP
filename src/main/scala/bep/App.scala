package bep

import bep.core._
import bep.free.{Fork, Free, Pure}
import bep.interp.Interpreter
import bep.interp.Interpreter.Result
import bep.syntax.Syntax._
import javax.xml.crypto.KeySelector.Purpose

/**
  * @author Adrian Mensing
  */
object App {

  private def length = letrec("length")
  private def f = letrec("f")

  private def L = Var("L")
  private def n = Var("n")
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
      f(n) :-
        matching(n)
          -> (Pattern(x), f(Plus(x, Num(1))))
          -> (Pattern(x), Num(0)),

      f(Num(0))

//      length(L) :-
//        matching(L)
//          -> (Pattern(Empty), Num(0))
//          -> (Pattern(Cons(x, xs)), Plus(Num(1), length(xs))),
//
//      length(Cons(1, Cons(2, Cons(3, Cons(4, Empty)))))
    ))

    val result = Interpreter.interp(code)

    println(result.asInstanceOf[Fork[Result]].patterns.foreach(p => println(p)))
//    result.values().toList.foreach(p => p.values().foreach(p2 => println(p2)))
//    println("result = " + result.get()._1)
  }
}
