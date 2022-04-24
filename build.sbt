ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organization := "com.github.buntec"
ThisBuild / organizationName := "buntec"

lazy val scalajsDomVersion = "2.1.0"

lazy val root = (project in file("."))
  .settings(publish / skip := true)
  .aggregate(snabbdom, examples)

lazy val snabbdom = (project
  .in(file("snabbdom")))
  .enablePlugins(ScalaJSPlugin, GitVersioning)
  .settings(
    name := "scala-js-snabbdom",
    git.useGitDescribe := true,
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion
    )
  )

lazy val examples = (project
  .in(file("examples")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scala-js-snabbdom-examples",
    publish / skip := true,
    scalaJSUseMainModuleInitializer := true,
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion
    )
  )
  .dependsOn(snabbdom)
