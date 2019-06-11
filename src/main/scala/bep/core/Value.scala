package bep.core

import bep.free.Free

abstract class Value

case class NumV(num: Int) extends Value
case class FunV(args: List[Expr], body: Expr) extends Value
case class ValV(x: Any, xs: List[Value]) extends Value
case class VarV(name: String) extends Value
case class LogicV(name: String) extends Value