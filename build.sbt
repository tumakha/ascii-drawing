name := "ascii-drawing"

version := "0.1"

scalaVersion := "2.12.8"

assemblyJarName in assembly := "drawing.jar"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")