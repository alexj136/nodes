package parser_util

import syntax._

sealed abstract class ParserResult
class ParserSuccess(p: Proc, names: Map[String, Name], nextName: Name)
  extends ParserResult
class ParserFailure(errors: List[SyntaxError]) extends ParserResult

class SyntaxError(start: (Int, Int), end: (Int, Int))
