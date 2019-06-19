package bep.interp

import bep.core.Expr.Var
import bep.core.Value

case class Environment(maps: Map[String, Value]) {
  def bind(v: Var, value: Value): Environment = bind(v.name, value)
  def bind(name: String, value: Value): Environment = Environment(maps + (name -> value))

  def get(name: String): Value = maps(name)

  def ++(env: Environment): Environment = Environment(maps ++ env.maps)
}
