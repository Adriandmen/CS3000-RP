package bep

import bep.core.{Equals, Exists, Expr, Num, NumV, Pattern, Plus, Val, ValV, Var}
import bep.interp.Interpreter
import bep.syntax.Syntax.{letrec, matching}
import org.scalatest.FunSuite

class InterpTest extends FunSuite {

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

  test("normal length method") {
    assertResult(
      ValV(4, Nil)
    ) {
      val code = Interpreter.concat(List(

        length(L) :-
          matching(L)
            -> (Pattern(Empty), Num(0))
            -> (Pattern(Cons(x, xs)), Plus(Num(1), length(xs))),

        length(Cons(1, Cons(2, Cons(3, Cons(4, Empty)))))
      ))

      Interpreter.interp(code).get()._1
    }
  }

  test("basic exists method") {
    assertResult(
      ValV(4, Nil)
    ) {
      val code = Interpreter.concat(List(
        f(n) :-
          matching(n)
            -> (Pattern(Num(3)), Num(5))
            -> (Pattern(Num(4)), Num(6)),

        Exists(n, Equals(Num(6), f(n)))
      ))

      Interpreter.interp(code).get()._1
    }
  }
}
