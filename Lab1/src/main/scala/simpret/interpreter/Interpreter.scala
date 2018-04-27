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
    x match {
        case Variable(id) =>
            store.get(id) match {
                case None => 
                    None
                case Some (r) => 
                    Some(r, store)
            }
        case BoolLit(b) =>
            None 
        case IntLit(i) =>
            None
        case CondExp(c, e1, e2) =>
            val r: AST = eval(c, store)
            r match {
                case BoolLit(true) =>
                    Some(e1, store)
                case BoolLit(false) =>
                    Some(e2, store)
            }
        case IsZeroExp(e) =>
            val r: AST = eval(e, store)
            r match {
                case IntLit(0) => 
                    Some(BoolLit(true), store)
                case _ =>
                    Some(BoolLit(false), store)
            }   
        case PlusExp(e1, e2) =>
            val r1: AST = eval(e1, store)
            val r2: AST = eval(e2, store)
            
            (r1, r2) match {
                case (IntLit(i1), IntLit(i2)) =>
                    Some(IntLit(i1 + i2), store)
                case (_, _) => None
            }
        case AssignExp(id, e) =>
            Some(e, store + (id -> e))
        case SeqExp(e1, e2) =>
            Some(e2, step(e1, store).get._2)
        case _ => 
            None
    }
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
