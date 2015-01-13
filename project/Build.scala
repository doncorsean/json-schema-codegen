import sbt._
import sbt.Keys._
import bintray.Plugin._
import bintray.Keys._

object Build extends Build {

  lazy val projectSettings = Seq(
    version := "0.0.1-SNAPSHOT",

    organization := "com.voxsupplychain",

    scalaVersion := "2.10.4",

    publishMavenStyle := false,

    repository in bintray := "vox-ivy",

    bintrayOrganization in bintray := Some("voxsupplychain")

  ) ++ bintraySettings

  lazy val root = Project(id = "json-schema-codegen", base = file("."))
    .settings(
      libraryDependencies ++= Seq(
        "com.voxsupplychain" %% "json-schema-parser" % "0.0.1-SNAPSHOT",
        "io.argonaut" %% "argonaut" % "6.0.4",
        "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
        "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
      ),
      packageLabels in bintray := Seq("json-schema", "code", "generator")
    )
    .settings(projectSettings: _*)


  lazy val sbtplugin: Project = Project(id = "json-schema-codegen-sbt",
    base = file("sbt-plugin")).dependsOn(root).settings(
      sbtPlugin := true
    )
    .settings(projectSettings: _*)

}