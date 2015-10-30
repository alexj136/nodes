package parser

import syntax._
import java_cup.runtime.ComplexSymbolFactory.Location

sealed abstract class ParserResult
case class ParserSuccess(p: Proc, names: Map[String, Name], nextName: Name)
  extends ParserResult
case class ParserFailure(errors: List[SyntaxError]) extends ParserResult

class SyntaxError(startRow: Int, startCol: Int, endRow: Int, endCol: Int) {
  def this(start: Location, end: Location) =
    this(start.getLine, start.getColumn, end.getLine, end.getColumn)
  override def toString: String = "Syntax error spanning from line " ++
    s"$startRow, column $startCol to line $endRow, column $endCol"
}
