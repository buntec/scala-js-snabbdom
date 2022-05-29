ThisBuild / tlBaseVersion := "0.2"

val scala213 = "2.13.8"
ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, "3.1.2")
ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / developers := List(
  tlGitHubDev("buntec", "Christoph Bunte"),
  tlGitHubDev("armanbilge", "Arman Bilge"),
  tlGitHubDev("davesmith00000", "Dave Smith")
)

ThisBuild / githubWorkflowBuildMatrixAdditions += "browser" -> List(
  "Chrome",
  "Firefox"
)
ThisBuild / githubWorkflowBuildSbtStepPreamble += s"set Global / useJSEnv := JSEnv.$${{ matrix.browser }}"
lazy val useJSEnv = settingKey[JSEnv]("Browser for running Scala.js tests")
Global / useJSEnv := JSEnv.Chrome
ThisBuild / Test / jsEnv := {
  import org.openqa.selenium.chrome.ChromeOptions
  import org.openqa.selenium.firefox.FirefoxOptions
  import org.scalajs.jsenv.selenium.SeleniumJSEnv
  useJSEnv.value match {
    case JSEnv.Chrome =>
      val options = new ChromeOptions()
      options.setHeadless(true)
      new SeleniumJSEnv(options)
    case JSEnv.Firefox =>
      val options = new FirefoxOptions()
      options.setHeadless(true)
      new SeleniumJSEnv(options)
  }
}

lazy val scalajsDomVersion = "2.1.0"
lazy val munitVersion = "0.7.29"
lazy val scalacheckVersion = "1.16.0"

lazy val root = tlCrossRootProject.aggregate(snabbdom, examples, benchmarks)

lazy val snabbdom = (project
  .in(file("snabbdom")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scala-js-snabbdom",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test
    )
  )

lazy val examples = (project
  .in(file("examples")))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .settings(
    name := "scala-js-snabbdom-examples",
    scalaJSUseMainModuleInitializer := true,
    Compile / fastLinkJS / scalaJSLinkerConfig ~= {
      import org.scalajs.linker.interface.ModuleSplitStyle
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(
            List("snabbdom.examples")
          )
        )
    }
  )
  .dependsOn(snabbdom)

lazy val benchmarks = (project
  .in(file("benchmarks")))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .settings(
    name := "scala-js-snabbdom-benchmarks",
    scalaJSUseMainModuleInitializer := false
  )
  .dependsOn(snabbdom)
