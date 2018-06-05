package simpret.typechecker

import simpret.parser._
import simpret.errors._

object Typechecker {
  // error handling helper functions
  def errUnknownAST(x: AST) = throw new UnknownASTTypecheckerException(x)
  def errExpectedType(ty_str: String, x: AST) = throw new NotExpectedTypeTypecheckerException(ty_str, x)
  def errVarUnbound(x: AST) = throw new VarUnboundTypecheckerException(x)
  def errAppArgument(ty_param: ASTTY, ty_arg: ASTTY, x: AST) = throw new ApplicationArgumentTypecheckerException(ty_param, ty_arg, x)
  def errBranch(ty1: ASTTY, ty2: ASTTY, x: AST) = throw new BranchMismatchTypecheckerException(ty1, ty2, x)
  def errArrowNotSame(ty_param: ASTTY, ty_res: ASTTY, x: AST) = throw new ArrowNotSameTypecheckerException(ty_param, ty_res, x)
  def errCons(eh_ty: ASTTY, et_lty: ASTTY, x: AST) = throw new ConsMismatchTypecheckerException(eh_ty, et_lty, x)
  def errProjTooSmall(x: AST) = throw new ProjectionTooSmallTypecheckerException(x)
  def errProjTooBig(length: Int, x: AST) = throw new ProjectionTooBigTypecheckerException(length, x)
  def errProjNotField(l: String, x: AST) = throw new ProjectionNotAFieldTypecheckerException(l, x)

  // the recursive typechecking relation
  def check(x: AST, env: Map[String, ASTTY] = Map.empty):ASTTY = {
    x match {
      case Variable(id) =>
        if (env.contains(id)) {
          env(id)
        } else {
          errVarUnbound(x)
        }
      case BoolLit(b) =>
        BoolTy
      case IntLit(i) =>
        IntTy
        
      case CondExp(c, e1, e2) =>
        (check(c, env), check(e1, env), check(e2, env)) match {
          case (BoolTy, t1, t2) =>
            t1 match {
              case `t2` => 
                t1
              case _ =>
                errBranch(t1, t2, x)
            }
          case (_, _, _) =>
            errExpectedType("BoolTy", x)
        }
      case PlusExp(e1, e2) =>
        (check(e1, env), check(e2, env)) match {
          case (IntTy, IntTy) =>
            IntTy
          case (t1, t2) =>
            errBranch(t1, t2, x)
        }
      case LtExp(e1, e2) =>
        (check(e1, env), check(e2, env)) match {
          case (IntTy, IntTy) =>
            BoolTy
          case (t1, t2) =>
            errBranch(t1, t2, x)
        }
      case UMinExp(e) =>
        check(e, env) match {
          case IntTy =>
            IntTy
          case t =>
            errExpectedType("IntTy", x)
        }
        
      case LamExp(id, ty, e) =>
        ArrowTy(ty, check(e, env + (id -> ty))) 
      case AppExp(e1, e2) =>
        (check(e1, env), check(e2, env)) match {
          case (ArrowTy(t1, t2), t3) =>
            t1 match {
              case `t3` => 
                t2
              case _ =>
                errAppArgument(t1, t3, x)
            }
          case (_, _) =>
            errUnknownAST(x) 
        }
      case LetExp(id, e1, e2) =>
        check(e2, env + (id -> check(e1, env)))
      case FixAppExp(e) =>
        check(e, env) match {
          case ArrowTy(t1, t2) =>
            t1 match {
              case `t2` => 
                t1
              case _ =>
                errArrowNotSame(t1, t2, x)
            }
          case _ => 
            BoolTy//errExpectedType("ArrowTy", x)
        }
      case TupleExp(el) =>
        TupleTy(el.map(check(_, env)))
      case ProjTupleExp(e, i) =>
        check(e, env) match {
          case TupleTy(tyl) =>
            if (i - 1 < 0) {
              errProjTooSmall(x)
            } else if (i - 1 < tyl.size) {
              tyl(i - 1)
            } else {
              errProjTooBig(i, x)
            } 
          case _ =>
            errExpectedType("TupleTy", e)
        }
      case RecordExp(em) =>
        RecordTy(em.map { case (k, v) => k -> check(v, env) })
      case ProjRecordExp(e, l) =>
        check(e, env) match {
          case RecordTy(tym) =>
            if (tym.contains(l)) {
              tym(l)
            } else {
              errProjNotField(l, x)
            } 
          case _ =>
            errExpectedType("RecordTy", x)
        }
      
      case NilExp(ty) =>
        ListTy(ty)
      case ConsExp(eh, et) =>
        (check(eh, env), check(et, env)) match {
          case (t1, ListTy(t2)) =>
            t1 match {
              case `t2` => 
                ListTy(t2)
              case _ =>
                errCons(t1, t2, x)
            }
          case (t1, t2) =>
            errCons(t1, t2, x)
        }
      case IsNilExp(e) =>
        check(e, env) match {
          case ListTy(ty) =>
            BoolTy
          case _ =>
            errExpectedType("ListTy", x)
        }
      case HeadExp(e) =>
        check(e, env) match {
          case ListTy(ty) =>
            ty
          case _ =>
            errExpectedType("ListTy", x)
        }
      case TailExp(e) =>
        check(e, env) match {
          case ListTy(ty) =>
            ListTy(ty)
          case _ =>
            errExpectedType("ListTy", x)
        }
    }
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[TypecheckingError, Unit] = {
    try {
      check(x)
      Right(Unit)
    } catch {
      case ex: TypecheckerException =>
        val msg = ex.message
        val x = ex.x
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(x)
        Left(TypecheckingError(Location.fromPos(x.pos), msg2))
    }
  }
}
