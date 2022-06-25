addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.4.11")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.10.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.1"
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.0-RC1")
