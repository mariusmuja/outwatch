enablePlugins(ScalaJSBundlerPlugin)

name := "outwatch-redux"

requireJsDomEnv in Test := true

publishArtifact in Test := false

parallelExecution in Test := false

useYarn := true

testFrameworks += new TestFramework("minitest.runner.Framework")
