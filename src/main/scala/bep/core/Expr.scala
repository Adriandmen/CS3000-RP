package bep.core

import bep.syntax.sCaseWithPattern

abstract class Expr

case class Seq(left: Expr, right: Expr)                       extends Expr
case class Num(num: Int)                                      extends Expr
case class Plus(left: Expr, right: Expr)                      extends Expr
case class Letrec(name: String, args: List[Expr], body: Expr) extends Expr
case class Call(name: String, args: List[Expr])               extends Expr {
  def :-(body: Expr): Expr = Letrec(name, args, body)
}

case class Val(name: Any, args: List[Expr])                   extends Expr
case class Var(name: String)                                  extends Expr

case class Case(x: Expr, patterns: List[(Pattern, Expr)])     extends Expr {
  def -> : sCaseWithPattern = sCaseWithPattern(x, patterns)
}
case class Pattern(p: Expr)                                   extends Expr


abstract class LogicExpr extends Expr
case class Equals(left: Expr, right: Expr)                    extends LogicExpr
case class Exists(v: Var, cond: LogicExpr)                    extends LogicExpr