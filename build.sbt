ThisBuild / scalaVersion := "3.8.1"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "named-tuple-parser-core",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test // TODO update
  )

lazy val demo = project
  .in(file("demo"))
  .dependsOn(core)
  .settings(
    name := "named-tuple-parser-demo",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "4.3.2",
      "com.lihaoyi" %% "ujson" % "4.3.2",
      "org.scalameta" %% "munit" % "1.1.0" % Test
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate(core, demo)
  .settings(
    name := "named-tuple-parser",
    publish / skip := true
  )
