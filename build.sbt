ThisBuild / organization := "io.dzik"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val algs = project
  .in(file("algs"))
  .settings(
    name := "algs",
    scalacOptions ++= Seq(
      "-Ymacro-annotations",
      "-language:postfixOps",
      "-language:higherKinds"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0-M4" % Test
    ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  )
