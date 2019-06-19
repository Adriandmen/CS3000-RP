package bep

import bep.Lib._
import bep.core.Value
import bep.core.Expr._
import bep.core.Pattern.{ValP, VarP}
import bep.core.Value.ValV
import bep.interp.Interpreter

/**
  * @author ${user.name}
  */
object App {


  def main(args : Array[String]): Unit = {

    val list1 = Cons(Num(1), Cons(Num(2), Empty))
    val list2 = Cons(Num(3), Cons(Num(4), Cons(Num(5), Empty)))

    val code = Letrec(
      List(
        (Var("append"), Function(Var("x"), Function(Var("ys"), Match(Var("x"), List(
          Case(ValP("Empty", Nil), Var("ys")),
          Case(ValP("Cons", List(VarP("a"), VarP("as"))), Val("Cons", List(Var("a"), Apply(Apply(Var("append"), Var("as")), Var("ys")))))
        ))))),

        (Var("map"), Function(Var("f"), Function(Var("list"), Match(Var("list"), List(
          Case(ValP("Empty", Nil), Val("Empty", Nil)),
          Case(ValP("Cons", List(VarP("l"), VarP("ls"))), Val("Cons", List(Apply(Var("f"), Var("l")), Apply(Apply(Var("map"), Var("f")), Var("ls")))))
        ))))),
      ),
      Apply(Apply(Var("map"), Function(Var("x"), Plus(Var("x"), Var("x")))), Cons(Num(1), Cons(Num(2), Cons(Num(3), Empty))))
    )

    println(formalized(Interpreter.interp(code)))
  }

  def formalized(value: Value): String = value match {
    case ValV(x, Nil) => x.toString
    case ValV(x, xs) => s"$x(${xs.map(formalized).mkString(", ")})"
    case x => x.toString
  }
}
