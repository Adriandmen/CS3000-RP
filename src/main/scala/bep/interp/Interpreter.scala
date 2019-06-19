package bep.interp

import bep.core.{Expr, Pattern, Value}
import bep.core.Expr._
import bep.core.Pattern.{ValP, VarP}
import bep.core.Value.{ThunkV, ValV}

import scala.language.implicitConversions

object Interpreter {

  implicit def value2id[A](value: Value): A = value.asInstanceOf[A]

  def interp[A](expr: Expr[A]): A = interp[A](expr, Environment(Map()))

  def interp[A](expr: Expr[A], env: Environment): A = {
    expr match {
      case Var(name) => env.get(name)
      case Val(value, args) => ValV(value, args.map(x => force(interp(x, env))))

      case Plus(left, right) => (force(interp(left, env)), force(interp(right, env))) match {
        case (ValV("Num", List(ValV(n1, Nil))), ValV("Num", List(ValV(n2, Nil)))) =>
          ValV("Num", List(ValV(n1.toString.toInt + n2.toString.toInt, Nil)))
      }

      case Letrec(bindings, body) =>
        val env1 = bindings.foldLeft(env)((e, a) => e.bind(a._1, interp(a._2, env)))
        interp(body, env1)

      case Function(arg, body) => ThunkV(Function(arg, body), env)

      case Match(arg, cases) =>
        val value = force(interp(arg, env))
        val result =
          cases
            .toStream
            .map(c => (c.body, doMatch(value, c.pattern, env)))
            .find(c => c._2 != null)
            .getOrElse(throw new Exception(s"Match exception, could not match $value with any of the case clauses"))

        interp(result._1, result._2)

      case Apply(e, arg) => interp(e, env) match {
        case ThunkV(Function(param, body), closureEnv) =>
          val env1 = (env ++ closureEnv).bind(param.asInstanceOf[Var].name, interp(arg, env))
          interp(body, env1)
      }
    }
  }

  def force(value: Value): Value = value match {
    case ThunkV(expr, env) => interp(expr, env)
    case v => v
  }

  def doMatch(left: Value, right: Pattern, env: Environment): Environment = {
    if (env == null)
      return null

    (left, right) match {
      case (ValV(l, ls), ValP(r, rs)) if l.equals(r) => ls.zip(rs).foldLeft(env)((e, t) => doMatch(t._1, t._2, e))
      case (v @ ValV(_, _), VarP(name)) => env.bind(name, v)
      case _ => null
    }
  }
}
