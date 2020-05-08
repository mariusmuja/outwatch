enablePlugins(ScalaJSBundlerPlugin)

name := "outwatch-util"

requireJsDomEnv in Test := true

version in installJsdom := "11.12.0"

publishArtifact in Test := false

parallelExecution in Test := false

useYarn := true

testFrameworks += new TestFramework("minitest.runner.Framework")
