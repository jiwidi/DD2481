name := "simpret"

version := "0.1"

scalaVersion := "2.12.5"


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"



// https://www.scala-sbt.org/1.x/docs/Triggered-Execution.html
// https://www.scala-sbt.org/1.0/docs/Howto-Triggered.html
watchSources += baseDirectory.value / "src" / "input.sipr"

