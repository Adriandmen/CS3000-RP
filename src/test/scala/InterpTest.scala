import bep.Lib
import bep.Lib._
import bep.core.Expr._
import bep.core.Pattern.{ValP, VarP}
import bep.core.Value.ValV
import bep.interp.Interpreter
import org.scalatest.FunSuite

class InterpTest extends FunSuite {

  test("Basic addition test") {
    assertResult(
      ValV("Num", List(ValV(15, Nil)))
    ) {
      val code = Letrec(
        List(
          Lib.sum
        ),
        Apply(Var("sum"), Cons(Num(1), Cons(Num(2), Cons(Num(3), Cons(Num(4), Cons(Num(5), Empty))))))
      )

      Interpreter.interp(code)
    }
  }

  test("Recursive Append method") {
    assertResult(
      Interpreter.interp(Cons(Num(1), Cons(Num(2), Cons(Num(3), Cons(Num(4), Cons(Num(5), Empty))))))
    ) {
      val list1 = Cons(Num(1), Cons(Num(2), Empty))
      val list2 = Cons(Num(3), Cons(Num(4), Cons(Num(5), Empty)))
      val code = Letrec(
        List(
          (Var("append"), Function(Var("x"), Function(Var("ys"), Match(Var("x"), List(
            Case(ValP("Empty", Nil), Var("ys")),
            Case(ValP("Cons", List(VarP("a"), VarP("as"))), Val("Cons", List(Var("a"), Apply(Apply(Var("append"), Var("as")), Var("ys")))))
          )))))
        ),
        Apply(Apply(Var("append"), list1), list2)
      )

      Interpreter.interp(code)
    }
  }

  test("Map function test") {
    assertResult(
      Interpreter.interp(Cons(Num(2), Cons(Num(4), Cons(Num(6), Empty))))
    ) {
      val code = Letrec(
        List(
          (Var("map"), Function(Var("f"), Function(Var("list"), Match(Var("list"), List(
            Case(ValP("Empty", Nil), Val("Empty", Nil)),
            Case(ValP("Cons", List(VarP("l"), VarP("ls"))), Val("Cons", List(Apply(Var("f"), Var("l")), Apply(Apply(Var("map"), Var("f")), Var("ls")))))
          )))))
        ),
        Apply(Apply(Var("map"), Function(Var("x"), Plus(Var("x"), Var("x")))), Cons(Num(1), Cons(Num(2), Cons(Num(3), Empty))))
      )

      Interpreter.interp(code)
    }
  }
}
