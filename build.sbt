lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "score.discord",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "redditbot",
    resolvers += "jcenter-bintray" at "http://jcenter.bintray.com",
    libraryDependencies ++= Seq(
      "net.dv8tion" % "JDA" % "3.5.1_344",
      "org.apache.commons" % "commons-lang3" % "3.5",
      "org.scala-lang.modules" %% "scala-async" % "0.9.6"
    )
  )
