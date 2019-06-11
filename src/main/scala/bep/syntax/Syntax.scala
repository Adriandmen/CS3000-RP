package bep.syntax

import bep.core.{Call, Case, Expr, Letrec, Pattern}

class Syntax {

}

object Syntax {
  def letrec(name: String): sDef = sDef(name)
  def matching(e: Expr): sCase = sCase(e: Expr)
}

case class sDef(name: String) extends Syntax {
  def apply(args: Expr*): Call = Call(name, args.toList)

  def call(args: Expr*): Call = Call(name, args.toList)
}

case class sLetRec(name: String, args: List[Expr]) extends Syntax {
  def :-(body: Expr): Expr = Letrec(name, args, body)
}

case class sCase(e: Expr) {
  def -> : sCaseWithPattern = sCaseWithPattern(e, List())
}

case class sCaseWithPattern(e: Expr, ps: List[(Pattern, Expr)]) {
  def apply(p: Pattern, b: Expr): Case = Case(e, ps ++ List((p, b)))
}
