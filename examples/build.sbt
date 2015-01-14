import json.schema.codegen.Plugin

name := "json-schema-codegen-examples"

Plugin.projectSettings

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.0.4",
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"
)
