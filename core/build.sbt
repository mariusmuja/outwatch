
enablePlugins(ScalaJSBundlerPlugin)

name := "outwatch-core"

libraryDependencies ++= Seq(
  "io.monix"        %%% "monix"       % "3.2.2",
  "org.scala-js"    %%% "scalajs-dom" % "1.0.0",
  "com.raquo"       %%% "domtypes"    % "0.10.0",
  "org.typelevel"   %%% "cats-core"   % "2.1.1",
  "io.monix" 	    %%% "minitest"    % "2.8.2" % Test
)

resolvers += Resolver.sonatypeRepo("public")


testFrameworks += new TestFramework("minitest.runner.Framework")


npmDependencies in Compile ++= Seq(
  "snabbdom" -> "0.7.2"
)

//skip in packageJSDependencies := false

requireJsDomEnv in Test := true

version in installJsdom := "16.2.2"

publishArtifact in Test := false

parallelExecution in Test := false

useYarn := true
