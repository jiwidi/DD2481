package simpret


import simpret.lexer._
import simpret.parser._
import simpret.errors._
import simpret.interpreter._
import org.scalatest.FunSuite
import java.io.File


class MainTestInterpreter extends FunSuite {

  def test_eval(filename: String, expected_result: Either[InterpreterError, AST]) = {
    assert(runcase(filename) === expected_result)
  }

  def test_var_capture(filename: String, expected_var_id: String, expected_expr: AST) = {
    try {
      runcase(filename)
      assert(false)
    } catch {
      case ex : VariableCapturedEvaluationException =>
        val var_id = ex.var_id
        val x = ex.subst_s
        assert(expected_var_id === var_id)
        assert(expected_expr === x)
    }
  }

  def getListOfFiles(dir: String, extensions: List[String]): List[File] = {
      new File(dir).listFiles.filter(_.isFile).toList.filter { file =>
          extensions.exists(file.getName.endsWith(_))
      }
  }

  def loadAst(filename: String): Either[InterpreterError, AST] = {
    for {
      input <- FileLoader(filename).right
      tokens <- Lexer(input).right
      ast <- Parser(tokens).right
    } yield (ast)
  }

  def singleAutoTest(in: File) = {
    var inFilename = in.getAbsolutePath()
    var stepFilename = inFilename + ".step"
    var ioError = true
    for {
      inAst <- loadAst(inFilename).right
      stepAst <- loadAst(stepFilename).right
    } yield {
      assert(Interpreter.step(inAst, Map.empty) == Some((stepAst, Map.empty)))
      ioError = false
    }
    assert(!ioError)
  }

  def runstep(filename: String, store: Map[String, AST]):
    Either[InterpreterError, Option[(AST, Map[String, AST])]] = {
    for {
      ast <- loadAst(filename).right
    } yield (Interpreter.step(ast, store))
  }

  def runcase(filename: String): Either[InterpreterError, AST] = {
    for {
      ast <- loadAst(filename).right
    } yield (Interpreter.eval(ast))
  }

}
