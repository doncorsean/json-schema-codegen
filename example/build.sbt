import json.schema.codegen.Plugin

name := "json-schema-codegen-example"

enablePlugins(Plugin)

Plugin.projectSettings

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.0.4"
)
