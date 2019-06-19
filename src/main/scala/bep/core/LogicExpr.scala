package bep.core

import bep.core.Expr.{Val, Var}

sealed trait LogicExpr extends Expr[Val]

object LogicExpr {
  case class Exists(arg: Var, cond: LogicExpr) extends LogicExpr
  case class Equals(left: Expr[Value], right: Expr[Value]) extends LogicExpr
}
