val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "FP project",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"           % "1.0.0"      % Test,
    "com.softwaremill.sttp.client4" %% "core"          % "4.0.0-M11",
    "io.circe"                       %% "circe-core"    % "0.15.0-M1",
    "io.circe"                       %% "circe-parser"  % "0.15.0-M1",
    "com.github.tototoshi"          %% "scala-csv"     % "1.3.10",
    "org.scalatest"                 %% "scalatest"     % "3.2.18" % Test
  )

  )
