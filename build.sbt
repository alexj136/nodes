name := "nodes"

sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
  val lexerFile = dir / "Lexer.java"
  scala.sys.process.Process(
    s"java -jar ./lib/jflex-1.6.0.jar -d $dir --nobak src/main/jflex/lexer.flex").!
  Seq(lexerFile)
}

sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
  val parserFile = dir / "Parser.java"
  val symFile = dir / "sym.java"
  scala.sys.process.Process(
    s"java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir $dir -parser Parser src/main/cup/parser.cup").!
  Seq(parserFile, symFile)
}
