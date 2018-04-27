package simpret

import simpret.errors._
import simpret.parser._


class MainTestLab2 extends MainTestInterpreter {
  val files = getListOfFiles("src/test/sint/lab2/steptests", List("sint")).sorted

  files.foreach { file =>
    test("step test " + file.getName()) {
      singleAutoTest(file)
    }
  }

  test("TestCaseLab2 0 square") {
    test_eval("src/test/sint/lab2/case_0_square_0.sint", Right(IntLit(1)))
    test_eval("src/test/sint/lab2/case_0_square_1.sint", Right(IntLit(12 * 12)))
    test_eval("src/test/sint/lab2/case_0_square_2.sint", Right(IntLit(15 * 15)))
  }

  test("TestCaseLab2 1 varcapture") {
    test_var_capture("src/test/sint/lab2/case_1_varcapt_0.sint", "x",
      LamExp("y", PlusExp(Variable("x"), Variable("y"))))

    test_var_capture("src/test/sint/lab2/case_1_varcapt_1.sint", "y",
      LamExp("x", CondExp(BoolLit(true), IntLit(5), Variable("y"))))
  }

  // this is how you add a test case
  test("TestCaseLab2 2 ABC") {
    // here you add the sub testcases
  }

  // this is how you add another test case
  test("TestCaseLab2 3 XYZ") {
    // here you add the corresponding sub testcases
  }
}
