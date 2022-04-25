ThisBuild / tlBaseVersion := "0.0"

val scala213 = "2.13.8"
ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, "3.1.2")
ThisBuild / organization := "com.github.buntec"
ThisBuild / organizationName := "buntec"

ThisBuild / tlFatalWarningsInCi := false

lazy val scalajsDomVersion = "2.1.0"

lazy val root = tlCrossRootProject.aggregate(snabbdom, examples)

lazy val snabbdom = (project
  .in(file("snabbdom")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scala-js-snabbdom",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion
    )
  )

lazy val examples = (project
  .in(file("examples")))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .settings(
    name := "scala-js-snabbdom-examples",
    scalaJSUseMainModuleInitializer := true,
  )
  .dependsOn(snabbdom)
