

version := "0.1.0-SNAPSHOT"
name := "kyo-experiments"

scalaVersion := "3.7.1"


val kioVersion = "1.0-RC1"

libraryDependencies += "io.getkyo" %% "kyo-prelude" % kioVersion
libraryDependencies += "io.getkyo" %% "kyo-core" % kioVersion
libraryDependencies += "io.getkyo" %% "kyo-direct" % kioVersion
libraryDependencies += "io.getkyo" %% "kyo-combinators" % kioVersion


scalacOptions ++= Seq(
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wconf:msg=(unused.*value|discarded.*value|pure.*statement):error",
  "-language:strictEquality"
)

scalafmtOnCompile := true


