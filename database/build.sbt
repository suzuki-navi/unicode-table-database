val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "generator",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

scalacOptions ++= Seq("-deprecation", "-Xmax-inlines", "64")

libraryDependencies += "io.circe" %% "circe-core" % "0.14.3"
libraryDependencies += "io.circe" %% "circe-generic" % "0.14.3"
libraryDependencies += "io.circe" %% "circe-parser" % "0.14.3"

