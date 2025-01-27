package bep.interp

import bep.`match`.Matcher
import bep.core._
import bep.free.{Flow, Fork, Free, Pure}

object Interpreter {

  type Result = (Value, Environment)

  var freshIndex = 0
  val MAX_INSTANTIATION_NO = 100

  val True = ValV("True", Nil)
  val False = ValV("False", Nil)

  def interp(expr: Expr): Free[Result] = interp(expr, Environment(Set()))

  def interp(expr: Expr, env: Environment): Free[Result] = {
    def pure(v: Value): Free[Result] = Pure((v, env))

    expr match {
      case Seq(left, right) => interp(left, env).bind(r1 => interp(right, r1._2))

      case Letrec(name, args, body) =>
        val function = FunV(args, body)
        Pure((function, env.bind(name, function)))

      case Call(name, args) =>
        val function = env.get(name).asInstanceOf[FunV]
        val innerVars = getVars(function.body) -- function.args.asInstanceOf[List[Var]]
        val freeVars = function.args ++ innerVars.toList
        val freshParams = function.args.map(_ => fresh())

        val env2 = Matcher.doMatch(args.map(a => interp(a, env).get()._1), freshParams.map(a => interp(a, env).get()._1), env)

        val functionBody = freeVars.zip(freshParams ++ innerVars.toList.map(_ => fresh())).foldLeft(function.body)((body, v) => subst(body, v._1.asInstanceOf[Var], v._2))

        interp(functionBody, env2)

      case Num(n) => pure(ValV(n, Nil))

      case Plus(left, right) =>
        interp(left, env).bind(r1 => interp(right, r1._2).bind(r2 => (r1._1, r2._1) match {
          case (ValV(n1, Nil), ValV(n2, Nil)) if n1.isInstanceOf[Int] && n2.isInstanceOf[Int] => pure(ValV(n1.asInstanceOf[Int] + n2.asInstanceOf[Int], Nil))
        }))

      case Val(v, vs) => pure(ValV(v, vs.map(x => interp(x, env).get()._1)))

      case Var(name) => pure(env.get(name))

      case Pattern(p) => interp(p, env)

      case Exists(v, cond) => interp(cond, env).bind(rs => Pure((rs._2.get(v.name), rs._2)))

      case Equals(left, right) =>
        val solutions = for {
          l <- interp(left, env).bfs()
          r <- interp(right, env).bfs()
          if l._1.equals(r._1)
        } yield (l, r)

        solutions.map(t => Pure[Result]((True, Matcher.doSingleMatch(t._1._1, t._2._1, Environment(t._1._2.binds ++ t._2._2.binds))))).head

      case Case(e, ps) =>
        interp(e, env).bind(v => Fork(v, ps, (value, patterns) => forkValues(value, patterns, env)))
    }
  }

  def fresh(): Var = {
    freshIndex += 1

    if (freshIndex > MAX_INSTANTIATION_NO)
      throw new InstantiationError("Maximum instantiation depth reached")

    Var(s"_$freshIndex")
  }

  def subst(expr: Expr, left: Var, right: Expr): Expr = expr match {
    case Var(x) if x.equals(left.name) => right
    case Var(x) => Var(x)
    case Pattern(p) => Pattern(subst(p, left, right))
    case Val(x, xs) => Val(x, xs.map(subst(_, left, right)))
    case Case(x, ps) => Case(subst(x, left, right), ps.map(p => (subst(p._1, left, right).asInstanceOf[Pattern], subst(p._2, left, right))))
    case Num(x) => Num(x)
    case Plus(l, r) => Plus(subst(l, left, right), subst(r, left, right))
    case Call(name, args) => Call(name, args.map(a => subst(a, left, right)))
    case x => throw new IllegalArgumentException(s"Could not substitute in expression $x")
  }

  def getVars(es: List[Expr]): Set[Var] = es.flatMap(e => getVars(e)).toSet
  def getVars(expr: Expr): Set[Var] = expr match {
    case Var(x) => Set(Var(x))
    case Case(x, ps) => getVars(x) ++ ps.flatMap(t => getVars(t._1) ++ getVars(t._2))
    case Pattern(p) => getVars(p)
    case Num(_) => Set()
    case Val(_, as) => getVars(as)
    case Call(_, as) => getVars(as)
    case Plus(l, r) => getVars(l) ++ getVars(r)
    case x => throw new IllegalArgumentException(s"Could not retrieve vars from $x")
  }

  def concat(exprs: List[Expr]): Expr = exprs match {
    case x :: Nil => x
    case x :: xs => Seq(x, concat(xs))
  }

  def forkValues(v: Result, ps: List[(Pattern, Expr)], env: Environment): Stream[Free[Result]] = {
    ps
      .toStream
      .map(x => (x._2, Matcher.doSingleMatch(interp(x._1, env).get(), v)))
      .filter(x => x._2 != null)
      .map(x => interp(x._1, x._2))
  }

  def printBinds(binds: List[Bind]): Unit = binds match {
    case (b @ Bind(n, VarV(_))) :: bs =>
      println(b)
      printBinds(bs)
    case (b @ Bind(n, ValV(_, _))) :: bs =>
      println(b)
      printBinds(bs)
    case _ :: bs => printBinds(bs)
    case Nil => Unit
  }
}
