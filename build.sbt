enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

name := "OutWatch"

normalizedName := "outwatch"


// use sbt-git for versioning
//version := "1.0.0-SNAPSHOT"
enablePlugins(GitVersioning)

organization := "com.github.mariusmuja"

scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.11.12", "2.12.7")


libraryDependencies ++= Seq(
  "io.monix"        %%% "monix"       % "3.0.0-RC2-840c090",
  "org.scala-js"    %%% "scalajs-dom" % "0.9.6",
  "com.raquo"       %%% "domtypes" % "0.9",
  "org.typelevel" %%% "cats-effect" % "1.0.0",
  "org.typelevel" %%% "cats-core" % "1.4.0",
  "io.monix" %%% "minitest" % "2.2.2" % Test
)

npmDependencies in Compile ++= Seq(
  "snabbdom" -> "0.7.2"
)

scalacOptions += {
  val local = baseDirectory.value.toURI
  val remote = s"https://raw.githubusercontent.com/mariusmuja/outwatch/${git.gitHeadCommit.value.get}/"
  s"-P:scalajs:mapSourceURI:$local->$remote"
}

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xfuture" ::
  "-Xlint" ::
  "-Ypartial-unification" ::
  "-Yno-adapted-args" ::
  "-Ywarn-infer-any" ::
  "-Ywarn-value-discard" ::
  "-Ywarn-nullary-override" ::
  "-Ywarn-nullary-unit" ::
  "-P:scalajs:sjsDefinedByDefault" ::
  Nil

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      "-Ywarn-extra-implicit" ::
      "-Ywarn-unused:-explicits,-implicits,_" ::
      Nil
    case _             =>
      "-Ywarn-unused" ::
      "-Xexperimental" ::   // SAM conversion
      Nil
  }
}

testFrameworks += new TestFramework("minitest.runner.Framework")

requiresDOM in Test := true

version in installJsdom := "11.12.0"

useYarn := true

publishMavenStyle := true

licenses += ("Apache 2", url("https://www.apache.org/licenses/LICENSE-2.0.txt"))

pomIncludeRepository := { _ => false }
