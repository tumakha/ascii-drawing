name := "ascii-drawing"

version := "0.1"

scalaVersion := "2.12.8"

assemblyJarName in assembly := "drawing.jar"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")