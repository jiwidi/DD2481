package simpret.interpreter

import simpret.parser._
import simpret.errors._


abstract class EvaluationException(val message: String, val x: AST,
                                   private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final class VariableCapturedEvaluationException(val var_id: String, val subst_s: AST,
                                                private val cause: Throwable = None.orNull)
  extends EvaluationException("variable (" + var_id + ") has been captured during substitution", subst_s, cause)

object Interpreter {
  def errVarCap(var_id: String, x: AST) = throw new VariableCapturedEvaluationException(var_id, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST): Boolean = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case LamExp(_,_,_) => true
    case NilExp(_) => true
    case ConsExp(eh, et) => isvalue(eh) & isvalue(et)
    case TupleExp(el) => el.forall(isvalue)
    case RecordExp(em) => em.values.forall(isvalue)
    case _ => false
  }

  /* function for determining the free variables of an expression */
  def freevars (x: AST) : List[String] = 
    x match {
      case Variable(id) => 
        List(id)
      case BoolLit(b) => 
        List()
      case IntLit(i) => 
        List()
      case CondExp(c, e1, e2) =>
        (freevars(c) ++ freevars(e1) ++ freevars(e2)).distinct
      case PlusExp(e1, e2) =>
        (freevars(e1) ++ freevars(e2)).distinct
      case LamExp(id, ty, e) =>
        freevars(e) diff List(id)
      case AppExp(e1, e2) =>
        freevars(e1) ++ freevars(e2)
    }

  /* function for carrying out a substitution */
  def subst (x: String, s: AST, t: AST) : AST = 
    if(freevars(s).contains(x)) {
      s match {
        case Variable(id) => 
          if(id == x) {
            t
          } else {
            s
          }
        case BoolLit(b) => 
          s
        case IntLit(i) => 
          s
        case CondExp(c, e1, e2) =>
          CondExp(subst(x, c, t), subst(x, e1, t), subst(x, e2, t))
        case PlusExp(e1, e2) =>
          PlusExp(subst(x, e1, t), subst(x, e2, t))
        case LamExp(id, ty, e) =>
          if(id == x || freevars(t).contains(id)) {
            errVarCap(id, t);
          } else {
            LamExp(id, ty, subst(x, e, t))
          }
        case AppExp(e1, e2) =>
          AppExp(subst(x, e1, t), subst(x, e2, t))
      }
    } else {
      s
    }

  /* evaluation function for taking one step at a time */
  def step(x: AST): Option[AST] = {
    x match {
      case BoolLit(b) =>
        None
      case IntLit(i) =>
        None
      case CondExp(c, e1, e2) =>
        c match {
          case BoolLit(true) =>
            Some(e1)
          case BoolLit(false) =>
            Some(e2)
          case _ =>
            step(c) match {
              case Some(a) =>
                Some(CondExp(a, e1, e2))
              case None =>
                None
            }
        }
      case PlusExp(e1, e2) =>
        (e1, e2) match {
          case (IntLit(i1), IntLit(i2)) =>
            Some(IntLit(i1 + i2))
          case (IntLit(i1), e2) =>
            step(e2) match {
              case Some(a) =>
                Some(PlusExp(IntLit(i1), a))
              case None =>
                None
            }
          case (e1, e2) =>
            step(e1) match {
              case Some(a) =>
                Some(PlusExp(a, e2))
              case None =>
                None
            }
        }
      case LamExp(id, ty, e) =>
        None
      case AppExp(e1, e2) =>
        (e1, step(e2)) match {
          case (LamExp(id, ty, e), None) =>
            Some(subst(id, e, e2))
          case (LamExp(id, ty, e), Some(a)) =>
            Some(AppExp(LamExp(id, ty, e), a))
          case (e1, Some(a)) =>
            Some(AppExp(e1, a))
          case (e1, None) =>
            step(e1) match {
              case None =>
                None
              case Some(a) =>
                Some(AppExp(a, e2))
            }
        }
    }
  }

  /* evaluation function to iterate the steps of evaluation */
  val printSteps = false
  def eval(x: AST): AST = {
    step(x) match {
      case None => {
        {
          // the rules couldn't be applied to reduce the expression further, now it's in normal form
          if (printSteps)
            print("\r\n========== done ==========\r\n")
          else
            ()
        }
        x
      }
      case Some(x1) => {
        {
          // we could reduce the expression by applying the rules
          if (printSteps)
            print("\r\n========== step ==========\r\n" + ASTPrinter.convToStr(x1))
          else
            ()
        }
        eval(x1)
      }
    }
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[EvaluationError, AST] = {
    try {
      Right(eval(x))
    } catch {
      case ex : EvaluationException =>
        val msg = ex.message
        val xe = ex.x
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(xe)
        Left(EvaluationError(Location.fromPos(xe.pos), msg2))
    }
  }
}
