package bep.core

sealed trait Pattern

object Pattern {
  case class ValP(value: Any, args: List[Pattern]) extends Pattern
  case class VarP(name: String) extends Pattern
}
