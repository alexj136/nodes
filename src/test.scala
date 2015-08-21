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

object ParserParAssociativityTest extends Test {
  override def description: String = {
    val a: String = "Test that the parser parses parallel"
    s"${a} composition with correct associativity"
  }
  override def assertion: Boolean = {
    val parse = Parser.parseString("end|end|end")
    Parallel(Parallel(End, End), End)
      .syntaxEquiv(Map.empty, parse._1, parse._2.map(_.swap))
  }
}

object RunTests {
  val testList: List[Test] = List(ParserParAssociativityTest)

  def main(args: Array[String]): Unit = {
    testList.foreach{ println }
    println(s"Tests run with ${testList.filter(!_.assertion).length} failures.")
  }
}
