package bep

import bep.core.Expr.{Apply, Case, Function, Match, Plus, Val, Var}
import bep.core.Pattern.{ValP, VarP}
import bep.core.{Expr, Value}

object Lib {
  type Util = (Var, Function)

  private def peano(num: Int): Val = num match {
    case 0 => Zero
    case _ => Succ(peano(num - 1))
  }

  // Helper constructors
  val Zero = Val("Zero", Nil)
  val Succ: Val => Val = x => Val("Succ", List(x))

  val Num: Int => Val = x => Val("Num", List(Val(x, Nil)))

  // Lists
  val Empty: Val = Val("Empty", Nil)
  val Cons: (Any, Expr[Value]) => Val = (x, y) => x match {
    case Val(_, _) => Val("Cons", List(x.asInstanceOf[Val], y))
    case _ => Val("Cons", List(Val(x, Nil), y))
  }

  val None = Val("None", Nil)

  // Predefined functions
  def length: Util = (Var("length"), Function(Var("x"), Match(Var("x"), List(
    Case(ValP("Cons", List(VarP("y"), VarP("ys"))), Plus(Apply(Var("length"), Var("ys")), Num(1))),
    Case(ValP("Empty", Nil), Num(0))
  ))))

  def sum: Util = (Var("sum"), Function(Var("x"), Match(Var("x"), List(
    Case(ValP("Cons", List(VarP("y"), VarP("ys"))), Plus(Apply(Var("sum"), Var("ys")), Var("y"))),
    Case(ValP("Empty", Nil), Num(0))
  ))))

  def append: Util = (Var("append"), Function(Var("x"), Function(Var("ys"), Match(Var("x"), List(
    Case(ValP("Empty", Nil), Var("ys")),
    Case(ValP("Cons", List(VarP("a"), VarP("as"))), Val("Cons", List(Var("a"), Apply(Apply(Var("append"), Var("as")), Var("ys")))))
  )))))

  def map: Util = (Var("map"), Function(Var("f"), Function(Var("list"), Match(Var("list"), List(
    Case(ValP("Empty", Nil), Val("Empty", Nil)),
    Case(ValP("Cons", List(VarP("l"), VarP("ls"))), Val("Cons", List(Apply(Var("f"), Var("l")), Apply(Apply(Var("map"), Var("f")), Var("ls")))))
  )))))
}
