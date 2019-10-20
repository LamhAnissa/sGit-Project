
name := "projetPoubelle"

version := "0.1"

scalaVersion := "2.13.1"


lazy val root = (project in file("."))
  .settings(
    name := "sgit",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0",

  )
parallelExecution in Test := false
assemblyJarName in assembly := "sgit_app.jar"
assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)