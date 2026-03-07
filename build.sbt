ThisBuild / scalaVersion := "3.8.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "named-tuple-parser",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test // TODO update
  )
