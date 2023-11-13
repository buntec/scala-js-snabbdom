addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.6.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.14.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.1"
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.0")
addSbtPlugin("com.armanbilge" % "sbt-bundlemon" % "0.1.3")
