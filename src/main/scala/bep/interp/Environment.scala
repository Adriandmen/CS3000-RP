package bep.interp

import bep.core.{Value, Var, VarV}

case class Bind(name: String, value: Value)
case class Environment(binds: Set[Bind]) {
  def bind(name: String, value: Value): Environment = Environment(binds + Bind(name, value))

  def get(v: Var): Value = get(v.name)
  def get(name: String): Value = {
    binds.find(b => b.name == name) match {
      case Some(bind) => bind.value
      case _ => VarV(name)
      //    case _ => throw new IllegalArgumentException(s"Could not retrieve value of $name")
    }
  }
}

