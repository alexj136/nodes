package test

import parser._
import syntax._


abstract class Test {
  def description: String
  def assertion: Boolean
  override def toString: String =
    s"Assertion ${if (this.assertion) "passed" else
      "FAILED"}: \'${this.description}\'."
}

class ParserTest(desc: String, testCode: String, testProc: Proc) extends Test {
  override def description: String = desc
  val code: String = testCode
  val proc: Proc = testProc
  override def assertion: Boolean =
    Parser.parseString(this.code)._1 %= this.proc
}

object RunTests {
  val testList: List[Test] = List(
    new ParserTest(
      "associativity of |",
      "end|end|end",
      Parallel(Parallel(End, End), End)))

  def main(args: Array[String]): Unit = {
    testList.foreach{ println }
    println(s"Tests run with ${testList.filter(!_.assertion).length} failures.")
  }
}
