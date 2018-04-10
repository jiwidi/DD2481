package simpret.interpreter

import simpret.parser._
import simpret.errors._


final case class EvaluationException(private val message: String, private val x: AST,
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


object Interpreter {
  def errFun(msg: String, x: AST) = throw new EvaluationException(msg, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST) = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case _ => false
  }

  /* evaluation function for taking one step at a time */
  def step(x: AST, store: Map[String, AST]): Option[(AST, Map[String, AST])] = {
    None
  }

  /* evaluation function to iterate the steps of evaluation */
  def eval(x: AST, store: Map[String, AST] = Map.empty): AST = {
    step(x, store) match {
      case None => x
      case Some ((x1, store1)) => eval(x1, store1)
    }
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[EvaluationError, AST] = {
    try {
      Right(eval(x))
    } catch {
      case EvaluationException (msg, xe, _) =>
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(xe)
        Left(EvaluationError(Location.fromPos(xe.pos), msg2))
    }
  }
}
