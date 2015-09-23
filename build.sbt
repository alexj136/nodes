name := "nodes"

seq(jflexSettings: _*)

sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
  val parserFile = dir / "Parser.java"
  val symFile = dir / "sym.java"
  scala.sys.process.Process(
    s"java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir $dir -parser Parser src/main/cup/parser.cup").!
  Seq(parserFile, symFile)
}
