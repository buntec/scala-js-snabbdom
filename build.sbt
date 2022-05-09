ThisBuild / tlBaseVersion := "0.0"

val scala213 = "2.13.8"
ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq("2.12.15", scala213, "3.1.2")
ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / tlSonatypeUseLegacyHost := false

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

lazy val root = tlCrossRootProject.aggregate(snabbdom, examples)

lazy val snabbdom = (project
  .in(file("snabbdom")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scala-js-snabbdom",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test
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
