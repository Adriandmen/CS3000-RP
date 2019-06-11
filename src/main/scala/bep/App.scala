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

  private def L = Var("L")
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
      f(n) :-
        matching(n)
          -> (Pattern(Num(3)), Num(5))
          -> (Pattern(Num(4)), Num(6))
          -> (Pattern(Num(8)), Num(6)),

      Exists(n, Equals(Num(6), f(n)))
    ))

    val result = Interpreter.interp(code)

    result.bfs().take(5).foreach(p => println(formalized(p)))
  }

  def formalized(result: Result): String = result._1 match {
    case ValV(n, Nil) => n.toString
    case x => x.toString
  }
}
