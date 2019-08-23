enablePlugins(ScalaJSBundlerPlugin)

name := "outwatch-redux"

requireJsDomEnv in Test := true

publishArtifact in Test := false

parallelExecution in Test := false

version in installJsdom := "15.1.1"

useYarn := true

testFrameworks += new TestFramework("minitest.runner.Framework")
