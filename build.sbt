ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

enablePlugins(ScalaJSPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "QuoTE-OT"
  )

libraryDependencies ++= Seq(
  "io.circe" %%% "circe-core" % "0.14.6",
  "io.circe" %%% "circe-generic" % "0.14.6",
  "io.circe" %%% "circe-parser" % "0.14.6"
)
