package simpret.parser

import simpret.errors.PrintingError


object ASTPrinter {
  // helper predicates to determine whether parentheses are needed
  def needParExpr(e: AST) = {
    e match {
      case SeqExp(_,_) => true
      case _ => false
    }
  }
  def needParTerm0(e: AST) = {
    e match {
      case CondExp(_,_,_) => true
      case IsZeroExp(_) => true
      case AssignExp(_,_) => true
      case _ => needParExpr(e)
    }
  }
  def needParTerm(e: AST) = {
    e match {
      case PlusExp(_, _) => true
      case _ => needParTerm0(e)
    }
  }

  /* helper function to print "with needed parentheses" */
  def convToStrWNP[A](needFunc: A=>Boolean, convFun:(A,Int)=>String, e: A, skipCnt: Int) = {
    val needPar = needFunc(e)
    val skipCnt_new = skipCnt + (if (needPar) 1 else 0)

    val str1 = if (needPar) "(" else ""
    val str2 = convFun(e, skipCnt_new)
    val str3 = if (needPar) ")" else ""
    str1 + str2 + str3
  }

  /* convert an AST to a String for pretty printing */
  def convToStr(x:AST, skipCnt: Int = 0):String = {
    val spaceStr = " "
    val indentSize = 2
    val indentStr = spaceStr * indentSize
    val newLine = "\r\n" + (spaceStr * skipCnt)

    x match {
      case Variable(id) =>
        id
      case BoolLit(b) =>
        if (b) "true" else "false"
      case IntLit(i) =>
        i.toString()

      case PlusExp(e1, e2) =>
        val str1 = convToStrWNP(needParTerm0, convToStr, e1, skipCnt)
        val strm = " + "
        val str2 = convToStrWNP(needParTerm0, convToStr, e2, skipCnt + str1.length() + strm.length())
        str1 + strm + str2

      case CondExp(c, e1, e2) =>
        val strm1 = "if "
        val str1 = convToStrWNP(needParExpr, convToStr, c, skipCnt + strm1.length())
        val strm2 = " then" + newLine + indentStr
        val str2 = convToStrWNP(needParExpr, convToStr, e1, skipCnt + indentStr.length())
        val strm3 = newLine + "else" + newLine + indentStr
        val str3 = convToStrWNP(needParExpr, convToStr, e2, skipCnt + indentStr.length())
        strm1 + str1 + strm2 + str2 + strm3 + str3

      case IsZeroExp(e) =>
        val strm = "iszero "
        val str1 = convToStrWNP(needParExpr, convToStr, e, skipCnt + strm.length())
        strm + str1

      case AssignExp(id, e) =>
        val str1 = id
        val strm = " := "
        val str2 = convToStrWNP(needParExpr, convToStr, e, skipCnt + id.length() + strm.length())
        str1 + strm + str2

      case SeqExp(e1, e2) =>
        val str1 = convToStr(e1, skipCnt)
        val strm = ";" + newLine
        val str2 = convToStr(e2, skipCnt)
        str1 + strm + str2

      case _ =>
        throw new Exception("Cannot print AST element: " + x)
    }
  }

  /* function to apply the AST printer */
  def apply(x: AST): Either[PrintingError, Unit] = {
    try {
      println("==========================================================")
      println("AST")
      println("==========================================================")
      println(convToStr(x))
      println("==========================================================")
      Right(Unit)
    } catch {
      case ex: Exception => Left(PrintingError(ex.getMessage()))
    }
  }
}
