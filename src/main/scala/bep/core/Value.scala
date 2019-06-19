package bep.core

import bep.interp.Environment

sealed trait Value

object Value {
  case class ValV(x: Any, xs: List[Value]) extends Value
  case class ThunkV[R](expr: Expr[R], env: Environment) extends Value
}
