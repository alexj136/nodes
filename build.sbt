name := "main"

TaskKey[Int]("Generate Lexer and Parser") :=
  scala.sys.process.Process("sh gen.sh").!
