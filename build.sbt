version := "0.1.0-SNAPSHOT"
name    := "kyo-experiments"

scalaVersion := "3.7.1"

val kioVersion = "1.0-RC1+13-428d4cfc-SNAPSHOT"

libraryDependencies += "io.getkyo" %% "kyo-prelude"     % kioVersion
libraryDependencies += "io.getkyo" %% "kyo-core"        % kioVersion
libraryDependencies += "io.getkyo" %% "kyo-direct"      % kioVersion
libraryDependencies += "io.getkyo" %% "kyo-combinators" % kioVersion

libraryDependencies += "co.fs2"        %% "fs2-core"    % "3.12.0"
libraryDependencies += "co.fs2"        %% "fs2-io"      % "3.12.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6.2"

scalacOptions ++= Seq(
    "-Wvalue-discard",
    "-Wnonunit-statement",
    "-Wconf:msg=(unused.*value|discarded.*value|pure.*statement):error",
    "-language:strictEquality"
)

scalafmtOnCompile := true
