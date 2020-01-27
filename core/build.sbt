
enablePlugins(ScalaJSBundlerPlugin)

name := "outwatch-core"

libraryDependencies ++= Seq(
  "io.monix"        %%% "monix"       % "3.1.0",
  "org.scala-js"    %%% "scalajs-dom" % "0.9.8",
  "com.raquo"       %%% "domtypes"    % "0.9.6",
  "org.typelevel"   %%% "cats-core"   % "2.0.0",
  "io.monix" 	    %%% "minitest"    % "2.7.0" % Test
)

resolvers += Resolver.sonatypeRepo("public")


testFrameworks += new TestFramework("minitest.runner.Framework")


npmDependencies in Compile ++= Seq(
  "snabbdom" -> "0.7.2"
)

skip in packageJSDependencies := false

requireJsDomEnv in Test := true

publishArtifact in Test := false

parallelExecution in Test := false

version in installJsdom := "15.1.1"

useYarn := true
