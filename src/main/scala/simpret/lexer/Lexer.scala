package simpret.lexer

import simpret.errors._
import scala.util.parsing.combinator._

// https://www.tutorialspoint.com/scala/scala_regular_expressions.htm


object Lexer extends RegexParsers {
  override def skipWhitespace = false

  /* comment and whitespace handling */
  def skip: Parser[Unit] = rep(whiteSpace | comment) ^^^ Unit
  def comment: Parser[Unit] = singleComment | multiComment
  def singleComment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ Unit
  def multiComment: Parser[Unit] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ Unit

  /* identifiers */
  def ident: Parser[TOKID] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r     ^^ { TOKID(_) }
  }

  /* literals */
  def literals: Parser[Token] =
    positioned (boolt | boolf | intlit)
  def boolt       = "true"         ^^ (_ => TOKBOOL(true) )
  def boolf       = "false"        ^^ (_ => TOKBOOL(false) )

  def intlit: Parser[TOKINT] = positioned {
    "0|([1-9][0-9]*)".r      ^^ { str => TOKINT(Integer.parseInt(str)) }
  }

  /* symbols */
  def symbols: Parser[Token] =
    positioned (condif | condthen | condelse |
      plus | lt | minus_tyarr |
      parl | parr |
      lam | dot | col_cons | let_letrec | in_tyint |
      fix |
      eq | comma | curl | curr |
      sqbl | sqbr | vbar | isnil | head | tail |
      tybool)

  def condif      = "if"           ^^ (_ => TOKIF)
  def condthen    = "then"         ^^ (_ => TOKTHEN)
  def condelse    = "else"         ^^ (_ => TOKELSE)

  def plus        = "+"            ^^ (_ => TOKPLUS)
  def lt          = "<"            ^^ (_ => TOKLT)
  def minus_tyarr = "-" ~ opt(">") ^^ {
    case _ ~ None    => TOKMINUS        // "-"
    case _ ~ Some(_) => TOKTYARROW      // "->"
  }

  def parl        = "("            ^^ (_ => TOKPARL)
  def parr        = ")"            ^^ (_ => TOKPARR)

  def lam         = "\\"           ^^ (_ => TOKLAM)
  def dot         = "."            ^^ (_ => TOKDOT)
  def col_cons    = ":" ~ opt(":") ^^ {
    case _ ~ None    => TOKCOL          // ":"
    case _ ~ Some(_) => TOKCONS         // "::"
  }
  def let_letrec  = "let" ~ opt("rec") ^^ {
    case _ ~ None    => TOKLET          // "let"
    case _ ~ Some(_) => TOKLETREC       // "letrec"
  }
  def in_tyint    = "in" ~ opt("t") ^^ {
    case _ ~ None    => TOKIN           // "in"
    case _ ~ Some(_) => TOKTYINT        // "int"
  }
  def fix         = "fix"          ^^ (_ => TOKFIX)

  def eq          = "="            ^^ (_ => TOKEQ)
  def comma       = ","            ^^ (_ => TOKCOM)
  def curl        = "{"            ^^ (_ => TOKCURL)
  def curr        = "}"            ^^ (_ => TOKCURR)

  def sqbl        = "["            ^^ (_ => TOKSQBL)
  def sqbr        = "]"            ^^ (_ => TOKSQBR)
  def vbar        = "|"            ^^ (_ => TOKVBAR)
  def isnil       = "isnil"        ^^ (_ => TOKISNIL)
  def head        = "hd"           ^^ (_ => TOKHEAD)
  def tail        = "tl"           ^^ (_ => TOKTAIL)

  def tybool      = "bool"         ^^ (_ => TOKTYBOOL)


  /* token stream */
  def tokens: Parser[List[Token]] = skip ~> rep1(token <~ skip) <~ eof

  def eof: Parser[String] = "\\z".r | failure("unexpected character")
  def token: Parser[Token] =
    positioned(literals | symbols | ident)

  def apply(input: String): Either[LexerError, List[Token]] = {
    parseAll(tokens, input) match {
      case NoSuccess(msg, next) => Left(LexerError(Location.fromPos(next.pos), msg))
      case Success(result, _) => Right(result)
    }
  }
}
