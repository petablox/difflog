import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.upenn.cis",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "QD",

    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
    libraryDependencies += "com.outr" %% "scribe" % "2.6.0",

    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
  )
