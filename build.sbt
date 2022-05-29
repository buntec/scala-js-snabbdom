ThisBuild / tlBaseVersion := "0.1"

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

ThisBuild / coverageScalacPluginVersion := "2.0.0-M6"
ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List(scala213),
    javas = githubWorkflowJavaVersions.value.toList,
    steps = List(WorkflowStep.Checkout)
      ++ WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList)
      ++ githubWorkflowGeneratedCacheSteps.value
      ++ List(
        WorkflowStep.Run(List("npm install")),
        WorkflowStep.Sbt(
          List(
            "set Global / useJSEnv := JSEnv.JSDOM",
            "coverage",
            "test",
            "coverageAggregate"
          )
        ),
        WorkflowStep.Run(List("bash <(curl -s https://codecov.io/bash)"))
      )
  )

lazy val useJSEnv = settingKey[JSEnv]("Browser for running Scala.js tests")
Global / useJSEnv := JSEnv.Chrome
ThisBuild / Test / jsEnv := {
  import org.openqa.selenium.chrome.ChromeOptions
  import org.openqa.selenium.firefox.FirefoxOptions
  import org.scalajs.jsenv.selenium.SeleniumJSEnv
  import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
  useJSEnv.value match {
    case JSEnv.Chrome =>
      val options = new ChromeOptions()
      options.setHeadless(true)
      new SeleniumJSEnv(options)
    case JSEnv.Firefox =>
      val options = new FirefoxOptions()
      options.setHeadless(true)
      new SeleniumJSEnv(options)
    case JSEnv.JSDOM =>
      new JSDOMNodeJSEnv()
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
