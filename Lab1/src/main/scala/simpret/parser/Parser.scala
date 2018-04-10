package simpret.parser

import simpret.errors._
import simpret.lexer._
import scala.util.parsing.combinator._
import scala.util.parsing.input._


// Background information:
//
// https://enear.github.io/2016/03/31/parser-combinators/
// https://github.com/enear/parser-combinators-tutorial
// https://github.com/scala/scala-parser-combinators
//
// https://stackoverflow.com/questions/11533547/operator-precedence-with-scala-parser-combinators
// http://bitwalker.org/posts/2013-08-10-learn-by-example-scala-parser-combinators/
// http://www.scala-lang.org/api/2.12.3/scala-parser-combinators/scala/util/parsing/combinator/RegexParsers.html
// http://nielssp.dk/2015/07/creating-a-scanner-using-parser-combinators-in-scala/
// http://www.scala-lang.org/files/archive/api/2.7.7/scala/util/parsing/input/


class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = if (atEnd) NoPosition else first.pos
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

object Parser extends Parsers {
  override type Elem = Token

  /*

  prog = expr0

  expr0 = expr ~ rep(semic_expr)
  semic_expr = SEMICOLON ~ expr

  expr = IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr | ISZERO ~ expr | x ~ ASSIGN ~ expr | term0

  term0 = atom ~ (rep(plus_term))
  plus_term = PLUS ~ atom

  atom = var | boollit | intlit | PARL ~ expr0 ~ PARR

  ------

  Print parentheses, when:
  CondExp/IsZeroExp/AssignExp -> SeqExp
  PlusExp -> SeqExp, CondExp, IsZeroExp, AssignExp

  */

  /* a whole program */
  def prog: Parser[AST] = {
    phrase(expr0)
  }

  def expr0: Parser[AST] = positioned {
    expr ~ rep(semic_expr) ^^ { case t1 ~ t2 => (t1 /: t2) { (acc,f) => f(acc) } }
  }

  def semic_expr: Parser[AST=>AST] = {
    TOKSC ~ expr ^^ { case _ ~ e => SeqExp(_, e) }
  }


  /* expressions */
  def expr: Parser[AST] = positioned {
    def cond: Parser[AST] = {
      TOKIF ~ expr ~ TOKTHEN ~ expr ~ TOKELSE ~ expr ^^ { case _ ~ c ~ _ ~ e1 ~ _ ~ e2 => CondExp(c, e1, e2) }
    }

    def iszero: Parser[AST] = {
      TOKISZERO ~ expr ^^ { case _ ~ e => IsZeroExp(e) }
    }

    def assign: Parser[AST] = {
      ident ~ TOKASSIGN ~ expr ^^ { case Variable(id) ~ _ ~ e => AssignExp(id, e) }
    }

    cond | iszero | assign | term0
  }


  /* terms */
  def term0: Parser[AST] = {
    def plus_term: Parser[AST=>AST] = {
      TOKPLUS ~ atom ^^ { case _ ~ e => PlusExp(_, e) }
    }

    atom ~ rep(plus_term) ^^ { case t1 ~ t2 => (t1 /: t2) { (acc,f) => f(acc) } }
  }

  /* atoms */
  def atom: Parser[AST] = positioned {
    ident | boollit | intlit | parexp0
  }

  def ident: Parser[AST] = {
    accept("identifier", { case _ @ TOKID(name) => Variable(name) })
  }

  def boollit: Parser[AST] = {
    accept("boolean literal", { case _ @ TOKBOOL(v) => BoolLit(v) })
  }

  def intlit: Parser[AST] = {
    accept("integer literal", { case _ @ TOKINT(v) => IntLit(v) })
  }

  def parexp0: Parser[AST] = {
    TOKPARL ~ expr0 ~ TOKPARR ^^ { case _ ~ e ~ _ => e }
  }


  /* function to apply the parser */
  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new TokenReader(tokens)
    prog(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(Location.fromPos(next.pos), msg))
      case Success(result, _) => Right(result)
    }
  }
}
