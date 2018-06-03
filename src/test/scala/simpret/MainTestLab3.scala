package simpret

import simpret.parser._


class MainTestLab3 extends MainTestInterpreter {
  val files = getListOfFiles("src/test/sint/lab3/steptests", List("sint")).sorted



  files.foreach { file =>
    test("step test " + file.getName()) {
      singleAutoTest(file)
    }
  }

/* Commented out by Nils
  val nottypable = getListOfFiles("src/test/sint/lab3/nottypable", List("sint")).sorted
  nottypable.foreach { file =>
    test("typing test " + file.getName()) {
      singleAutoTypeTest(file)
    }
  }
*/

  test("TestCaseLab3 0 recfun") {
    test_eval("src/test/sint/lab3/case_0_recfun_0.sint", Right(IntLit(27)))
  }

  test("TestCaseLab3 1 extensions 0") {
    test_eval_text("src/test/sint/lab3/case_1_extensions_0.sint",
      "(4, [5, 2 | int ], 5, [([int]->(int, bool, int))->{abc:int, cdf:int->int->bool}])")
  }

  test("TestCaseLab3 1 extensions 1") {
    test_eval_text("src/test/sint/lab3/case_1_extensions_1.sint",
      "hd [int]")
  }

  test("TestCaseLab3 1 extensions 2") {
    test_eval_text("src/test/sint/lab3/case_1_extensions_2.sint",
      "3")
  }

  test("TestCaseLab3 1 extensions 3") {
    test_eval_text("src/test/sint/lab3/case_1_extensions_3.sint",
      "1")
  }

  test("TestCaseLab3 1 extensions 4") {
    test_eval_text("src/test/sint/lab3/case_1_extensions_4.sint",
      "[8,9,10|int]")
  }

  test("TestCaseLab3 2 letin") {
    test_eval("src/test/sint/lab3/case_2_letin_0.sint", Right(IntLit(10)))
  }
  
  /*
  // this is how you add a test case
  test("TestCaseLab3 3 ABC") {
    // here you add the sub testcases
  }

  // this is how you add another test case
  test("TestCaseLab3 4 XYZ") {
    // here you add the corresponding sub testcases
  }
  */
}
