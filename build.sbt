name := "nodes"

scalaVersion := "2.10.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.10" % "2.3.9"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

scalacOptions ++= Seq("-feature", "-deprecation", "unchecked")

sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
  val lexerFile = dir / "Lexer.java"
  scala.sys.process.Process(
    s"java -jar ./lib/jflex-1.6.1.jar -d $dir --nobak src/main/flex/lexer.flex").!
  Seq(lexerFile)
}

sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
  val parserFile = dir / "Parser.java"
  val symFile = dir / "sym.java"
  scala.sys.process.Process(
    s"java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir $dir -parser Parser -locations src/main/cup/parser.cup").!
  Seq(parserFile, symFile)
}
