package bep.core

trait Expr[-R]

object Expr {
  case class Var(name: String) extends Expr[Value]
  case class Val(value: Any, args: List[Expr[Value]]) extends Expr[Value]
  case class Function(arg: Var, body: Expr[Value]) extends Expr[Value]
  case class Letrec(bindings: List[(Var, Function)], body: Expr[Value]) extends Expr[Value]
  case class Case(pattern: Pattern, body: Expr[Value]) extends Expr[Value]
  case class Match(arg: Expr[Value], cases: List[Case]) extends Expr[Value]
  case class Apply(body: Expr[Value], arg: Expr[Value]) extends Expr[Value]

  case class Plus(left: Expr[Value], right: Expr[Value]) extends Expr[Value]
}
