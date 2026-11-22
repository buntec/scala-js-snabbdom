addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.8.7")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.23.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.1"
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.1"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("com.armanbilge" % "sbt-bundlemon" % "0.1.4")
