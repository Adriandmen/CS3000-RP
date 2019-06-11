package bep.interp

import bep.core.{ValV, Value, Var, VarV}

case class Bind(name: String, value: Value)
case class Environment(binds: Set[Bind]) {
  def bind(name: String, value: Value): Environment = Environment(binds + Bind(name, value))

  def get(v: Var): Value = get(v.name)
  def get(name: String): Value = {
    val bs = binds.filter(b => b.name == name)

    bs.find {
      case Bind(_, ValV(_, _)) => true
      case _ => false
    }.getOrElse(bs.headOption.getOrElse(Bind(name, VarV(name)))).value
  }

//  def unify(): Environment = {
//    binds.foreach()
//  }
}

