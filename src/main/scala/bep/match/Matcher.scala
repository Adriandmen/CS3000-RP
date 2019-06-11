package bep.`match`

import bep.core.{ValV, Value, VarV}
import bep.interp.Environment

object Matcher {
  type Result = (Value, Environment)

  def doMatch(left: List[Result], right: List[Result]): Environment = doMatch(left.map(_._1), right.map(_._1), right.head._2)
  def doMatch(left: List[Value], right: List[Value], env: Environment): Environment = (left, right) match {
    case (l :: ls, r :: rs) => doMatch(ls, rs, doSingleMatch(l, r, env))
    case (Nil, Nil) => env
  }

  def doSingleMatch(left: Result, right: Result): Environment = doSingleMatch(left._1, right._1, right._2)
  def doSingleMatch(left: Value, right: Value, env: Environment): Environment = {
//    println(s"matching:\n - $left \n - $right")
    (left, right) match {
      case (ValV(n1, ns1), ValV(n2, ns2)) if n1.equals(n2) => doMatch(ns1, ns2, env)
      case (VarV(name), v @ ValV(_, _)) => env.bind(name, v)
      case (v @ ValV(_, _), VarV(name)) => env.bind(name, v)
      case (VarV(n1), VarV(n2)) => env.bind(n1, VarV(n2)).bind(n2, VarV(n1))
      case _ => null
    }
  }
}
