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

  test("TestCaseLab1 0 COND 0") {
    test_eval("src/test/sint/lab1/case_0_cond_0.sint", Right(IntLit(11)))
  }

  test("TestCaseLab1 0 COND 1") {
    test_eval("src/test/sint/lab1/case_0_cond_1.sint", Right(IntLit(12)))
  }

  // This testcase doesn't make sense in lab 3 as it would fail typing
  // test("TestCaseLab1 0 COND 2") {
  //   // here evaluation gets stuck
  //   test_eval("src/test/sint/lab1/case_0_cond_2.sint", Right(CondExp(IntLit(-1),IntLit(11),IntLit(12))))
  // }

  // this is how you add a test case
  test("TestCaseLab1 1 ABC") {
    // here you add the sub testcases
  }

  // this is how you add another test case
  test("TestCaseLab1 2 XYZ") {
    // here you add the corresponding sub testcases
  }
}
