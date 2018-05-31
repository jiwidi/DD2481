package simpret

import simpret.errors._
import simpret.parser._


class MainTestLab1 extends MainTestInterpreter {
  val files = getListOfFiles("src/test/sint/lab1/steptests", List("sint")).sorted

  files.foreach { file =>
    test("step test " + file.getName()) {
      singleAutoTest(file)
    }
  }

  test("TestCaseLab1 0 COND") {
    test_eval("src/test/sint/lab1/case_0_cond_0.sint", Right(IntLit(11)))
    test_eval("src/test/sint/lab1/case_0_cond_1.sint", Right(IntLit(12)))
    // here evaluation gets stuck
    test_eval("src/test/sint/lab1/case_0_cond_2.sint", Right(CondExp(IntLit(-1),IntLit(11),IntLit(12))))
  }

  test("Dref-0.sint") {
    assert(runstep("src/test/sint/lab1/Deref-0.sint", Map("x" -> IntLit(3)))
      == Right(Some((PlusExp(IntLit(42),IntLit(3)),Map("x" -> IntLit(3))))))
  }

  test("AssignVal-0.sint") {
    assert(runstep("src/test/sint/lab1/AssignVal-0.sint", Map.empty)
      == Right(Some((BoolLit(true),Map("x" -> BoolLit(true))))))
  }

  test("AssignVal-1.sint") {
    assert(runstep("src/test/sint/lab1/AssignVal-1.sint", Map.empty)
      == Right(Some((IntLit(3),Map("x" -> IntLit(3))))))
  }

  test("AssignVal-2.sint") {
    assert(runstep("src/test/sint/lab1/AssignVal-2.sint", Map.empty)
      == Right(Some((SeqExp(IntLit(3),Variable("x")),Map("x" -> IntLit(3))))))
  }



  // this is how you add a test case
  test("TestCaseLab1 1 ABC") {
    // here you add the sub testcases
  }

  // this is how you add another test case
  test("TestCaseLab1 2 XYZ") {
    // here you add the corresponding sub testcases
  }
}
