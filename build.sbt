name := "nodes"

scalaVersion := "2.10.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.10" % "2.3.9"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

scalacOptions ++= Seq("-feature", "-deprecation", "unchecked")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")
