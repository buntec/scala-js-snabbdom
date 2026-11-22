ThisBuild / tlBaseVersion := "0.2"

val scala213 = "2.13.18"
val scala3 = "3.3.9"
ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / startYear := Some(2022)

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
    cond = Some("matrix.project == 'rootJS' && matrix.browser == 'Chrome'")
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
          UseRef.Public("actions", "setup-node", "v6"),
          params = Map("node-version" -> "22", "cache" -> "npm")
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
        WorkflowStep.Use(
          UseRef.Public("codecov", "codecov-action", "v5"),
          params = Map("token" -> "${{ secrets.CODECOV_TOKEN }}")
        )
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
      options.addArguments("--headless=new")
      new SeleniumJSEnv(options)
    case JSEnv.Firefox =>
      val options = new FirefoxOptions()
      options.addArguments("-headless")
      new SeleniumJSEnv(options)
    case JSEnv.JSDOM =>
      new JSDOMNodeJSEnv()
  }
}

lazy val scalajsDomVersion = "2.8.1"
lazy val scalacheckVersion = "1.19.0"
lazy val munitVersion = "1.3.6"

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
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1" % Test
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
