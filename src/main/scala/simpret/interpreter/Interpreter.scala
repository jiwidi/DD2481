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
      case LtExp(e1, e2) =>
        (freevars(e1) ++ freevars(e2)).distinct
      case UMinExp(e) =>
        freevars(e)
        
      case LamExp(id, ty, e) =>
        freevars(e) diff List(id)
      case AppExp(e1, e2) =>
        (freevars(e1) ++ freevars(e2)).distinct
      case LetExp(id, e1, e2) =>
        (freevars(e1) ++ freevars(e2)).distinct diff List(id)
      case FixAppExp(e) =>
        freevars(e)
        
      case TupleExp(el) =>
        el.flatMap(freevars).distinct
      case ProjTupleExp(e, i) =>
        freevars(e)
      case RecordExp(em) =>
        em.values.toList.flatMap(freevars).distinct
      case ProjRecordExp(e, l) =>
        freevars(e)
      
      case NilExp(ty) =>
        List()
      case ConsExp(eh, et) =>
        (freevars(eh) ++ freevars(et)).distinct
      case IsNilExp(e) =>
        freevars(e)
      case HeadExp(e) =>
        freevars(e)
      case TailExp(e) =>
        freevars(e)
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
        case LtExp(e1, e2) =>
          LtExp(subst(x, e1, t), subst(x, e2, t))
        case UMinExp(e) =>
          UMinExp(subst(x, e, t))
          
        case LamExp(id, ty, e) =>
          if(id == x || freevars(t).contains(id)) {
            errVarCap(id, t);
          } else {
            LamExp(id, ty, subst(x, e, t))
          }
        case AppExp(e1, e2) =>
          AppExp(subst(x, e1, t), subst(x, e2, t))
        case LetExp(id, e1, e2) =>
          //if(id == x || freevars(t).contains(id)) {
          //  errVarCap(id, t);
          //} else {
            LetExp(id, subst(x, e1, t), subst(x, e2, t))
          //}
        case FixAppExp(e) =>
          FixAppExp(subst(x, e, t))
          
        case TupleExp(el) =>
          TupleExp(el.map(subst(x, _, t)))
        case ProjTupleExp(e, i) =>
          ProjTupleExp(subst(x, e, t), i)
        case RecordExp(em) =>
          em foreach (ee => subst(x, ee._2, t))
          RecordExp(em)
        case ProjRecordExp(e, l) =>
          ProjRecordExp(subst(x, e, t), l)
          
        case NilExp(ty) =>
          s
        case ConsExp(eh, et) =>
          ConsExp(subst(x, eh, t), subst(x, et, t))
        case IsNilExp(e) =>
          IsNilExp(subst(x, e, t))
        case HeadExp(e) =>
          HeadExp(subst(x, e, t))
        case TailExp(e) =>
          TailExp(subst(x, e, t))
      }
    } else {
      s
    }

  /* evaluation function for taking one step at a time */
  def step(x: AST): Option[AST] = {
    x match {
      case Variable(id) =>
        None
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
            step(c).map(CondExp(_, e1, e2))
        }
      case PlusExp(e1, e2) =>
        (e1, e2) match {
          case (IntLit(i1), IntLit(i2)) =>
            Some(IntLit(i1 + i2))
          case (IntLit(i1), e2) =>
            step(e2).map(PlusExp(IntLit(i1), _))
          case (e1, e2) =>
            step(e1).map(PlusExp(_, e2))
        }
      case LtExp(e1, e2) =>
        (e1, e2) match {
          case (IntLit(i1), IntLit(i2)) =>
            Some(BoolLit(i1 < i2))
          case (IntLit(i1), e2) =>
            step(e2).map(LtExp(IntLit(i1), _))
          case (e1, e2) =>
            step(e1).map(LtExp(_, e2))
        }
      case UMinExp(e) =>
        e match {
          case IntLit(i) =>
            Some(IntLit(-i))
          case e =>
            step(e).map(UMinExp(_))
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
            step(e1).map(AppExp(_, e2))
        }
      case LetExp(id, e1, e2) =>
        step(e1) match {
          case Some(a) =>
            Some(LetExp(id, a, e2))
          case None =>
            Some(subst(id, e2, e1))
        }
      case FixAppExp(e) =>
        e match {
          case LamExp(id, ty, ee) =>
            Some(subst(id, ee, FixAppExp(e)))
          case e =>
            step(e).map(FixAppExp(_))
        }
        
      case TupleExp(el) =>
        val pre = el.takeWhile(isvalue(_)) 
        if (pre.size < el.size) {
          step(el(pre.size)) match {
            case Some(a) =>
              Some(TupleExp(el.patch(pre.size, List(a), 1)))
            case None =>
              None
          }
        } else {
          None
        }
      case ProjTupleExp(e, i) =>
        e match {
          case TupleExp(el) =>
            if (isvalue(el(i - 1))) {
              Some(el(i - 1))
            } else {
              step(e)
            }
          case e =>
            step(e).map(ProjTupleExp(_, i))
        }
      case RecordExp(em) =>
        val pre = em.dropWhile { case (k, v) => isvalue(v) }
        pre.head match {
          case (k, v) =>
            step(v) match {
              case Some(a) =>
                Some(RecordExp(em + (k -> a)))
              case None =>
                None
            }
          case _ =>
            None
        }
      case ProjRecordExp(e, l) =>
        e match {
          case RecordExp(em) =>
            if (isvalue(em(l))) {
              Some(em(l))
            } else {
              step(e)
            }
          case e =>
            step(e).map(ProjRecordExp(_, l))
        }
        
      case NilExp(ty) =>
        None
      case ConsExp(eh, et) => 
        step(eh) match {
          case Some(a) =>
            Some(ConsExp(a, et))
          case None =>
            step(et) match {
              case Some(b) =>
                Some(ConsExp(eh, b))
              case None =>
                None
            }
        }
      case IsNilExp(e) =>
        e match {
          case NilExp(ty) =>
            Some(BoolLit(true))
          case ConsExp(eh, et) =>
            Some(BoolLit(false))
          case e =>
            step(e).map(HeadExp(_))
        }
      case HeadExp(e) =>
        e match {
          case ConsExp(eh, et) =>
            if (isvalue(e)) {
              Some(eh)
            } else {
              step(e).map(HeadExp(_))
            }
          case e =>
            step(e).map(HeadExp(_))
        }
      case TailExp(e) =>
        e match {
          case ConsExp(eh, et) =>
            if (isvalue(e)) {
              Some(et)
            } else {
              step(e).map(TailExp(_))
            }
          case e =>
            step(e).map(TailExp(_))
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
