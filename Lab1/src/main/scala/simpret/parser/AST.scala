package simpret.parser


import scala.util.parsing.input.Positional


trait AST extends Positional

case class Variable(id: String) extends AST
case class BoolLit(b: Boolean) extends AST
case class IntLit(i: Int) extends AST
case class CondExp(c: AST, e1: AST, e2: AST) extends AST
case class IsZeroExp(e: AST) extends AST
case class PlusExp(e1: AST, e2: AST) extends AST
case class AssignExp(id: String, e: AST) extends AST
case class SeqExp(e1: AST, e2: AST) extends AST
