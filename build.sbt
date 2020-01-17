// use sbt-git for versioning
enablePlugins(GitVersioning)

inThisBuild(Seq(
  organization := "com.github.mariusmuja",
  scalaVersion := crossScalaVersions.value.head,
  version := "1.0.0-RC1-" + git.gitHeadCommit.value.get.take(8),
  crossScalaVersions := Seq("2.13.1", "2.12.10"),

  scalacOptions += {
    val local = baseDirectory.value.toURI
    val remote = s"https://raw.githubusercontent.com/mariusmuja/outwatch/${git.gitHeadCommit.value.get}/"
    s"-P:scalajs:mapSourceURI:$local->$remote"
  },

  scalacOptions ++=
    "-encoding" :: "UTF-8" ::
      "-unchecked" ::
      "-deprecation" ::
      "-explaintypes" ::
      "-feature" ::
      "-language:_" ::
      "-Xcheckinit" ::
      "-Xlint" ::
      "-Ywarn-value-discard" ::
//      "-P:scalajs:sjsDefinedByDefault" ::
      Nil,

  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        "-Ywarn-extra-implicit" ::
          "-Ywarn-unused:-explicits,-implicits,_" ::
          Nil
      case Some((2, 12)) =>
      "-Xfuture" ::
        "-Ywarn-extra-implicit" ::
          "-Ywarn-unused:-explicits,-implicits,_" ::
      	"-Ypartial-unification" ::
      "-Yno-adapted-args" ::
      "-Ywarn-infer-any" ::
      "-Ywarn-nullary-override" ::
      "-Ywarn-nullary-unit" ::
          Nil
      case _             =>
      "-Xfuture" ::
        "-Ywarn-unused" ::
          "-Xexperimental" ::   // SAM conversion
      	"-Ypartial-unification" ::
      "-Yno-adapted-args" ::
      "-Ywarn-infer-any" ::
      "-Ywarn-nullary-override" ::
      "-Ywarn-nullary-unit" ::
          Nil
    }
  },
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
))


lazy val core = project.in(file("core"))

lazy val util = project.in(file("util"))
  .dependsOn(core % "compile->compile;test->test")

lazy val router = project.in(file("router"))
  .dependsOn(util)

lazy val redux = project.in(file("redux"))
  .dependsOn(core % "compile->compile;test->test")


lazy val outwatch = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(name := "outwatch")
  .dependsOn(core, util, router, redux)
  .aggregate(core, util, router, redux)
