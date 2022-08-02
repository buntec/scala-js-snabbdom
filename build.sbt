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

ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "browser" -> List("Chrome", "Firefox")
ThisBuild / githubWorkflowBuildSbtStepPreamble += s"set Global / useJSEnv := JSEnv.$${{ matrix.browser }}"

ThisBuild / githubWorkflowBuild +=
  WorkflowStep.Sbt(
    List("bundleMon"),
    name = Some("Monitor artifact size"),
    cond = Some("matrix.project == 'rootJS'")
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List(scala213),
    javas = githubWorkflowJavaVersions.value.toList,
    steps = List(WorkflowStep.CheckoutFull)
      ++ WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList)
      ++ githubWorkflowGeneratedCacheSteps.value
      ++ List(
        WorkflowStep.Use(
          UseRef.Public("actions", "setup-node", "v3"),
          params = Map("node-version" -> "16", "cache" -> "npm")
        ),
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
lazy val scalacheckVersion = "1.16.0"
lazy val munitVersion = "1.0.0-M5"

lazy val root = tlCrossRootProject.aggregate(snabbdom, examples, benchmarks)

lazy val snabbdom = (project
  .in(file("snabbdom")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scala-js-snabbdom",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test,
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0" % Test
    )
  )

lazy val examples = (project
  .in(file("examples")))
  .enablePlugins(ScalaJSPlugin, BundleMonPlugin, NoPublishPlugin)
  .settings(
    name := "scala-js-snabbdom-examples",
    coverageEnabled := false,
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
