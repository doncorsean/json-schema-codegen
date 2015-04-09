package json.schema.codegen

import java.io.{FilenameFilter, File}
import java.nio.file.Path

import json.schema.parser.JsonSchemaParser
import sbt.Keys._
import sbt.{Def, _}

object Plugin extends sbt.AutoPlugin {

  override def trigger = allRequirements


  object autoImport {
    lazy val jsonCodegen = TaskKey[Seq[File]]("json-schema-codegen", "Generate code from Json-Schema")
  }

  import autoImport._

  def jsonGenSettings(config: Configuration): Seq[Def.Setting[_]] = Seq(

    sourceDirectory in jsonCodegen in config <<= (sourceDirectory in config)(src => src / "json-schema"),

    sources in jsonCodegen in config <<= (sourceDirectory in jsonCodegen in config) map {
      (schemaSrc) =>
        val jsonFiles = (schemaSrc ** GlobFilter("*.json")).get

        jsonFiles
    },

    sourceGenerators in config <+= (jsonCodegen in config),

    jsonCodegen in config <<=
      (sourceManaged in config, streams in config, sources in jsonCodegen in config) map {
        (dir, s, jsonFiles) =>

          val log = s.log("json-schema-codegen")

          val codegen = new CodeGen {
            override def debug(s: => String): Unit = log.debug(s)

            override def info(s: => String): Unit = log.info(s)

            override def error(s: => String): Unit = log.error(s)
          }

          val cachedFun = FileFunction.cached(s.cacheDirectory / "json-schema",
            FilesInfo.lastModified, /* inStyle */
            FilesInfo.exists) /* outStyle */ {
            (jsonSchemas: Set[File]) =>

              val genRoot: Path = dir.toPath

              log.info(s"Generating code using $jsonSchemas in $genRoot")
              val generator = if (true) codegen.gen(JsonSchemaParser, jsonSchemas.toSeq)(_) else codegen.gen(new JsonSchemaParser[Float], jsonSchemas.toSeq)(_)
              generator(genRoot).fold(
                e => throw new IllegalArgumentException(s"Failed code generation in $jsonSchemas: $e "),
                p => p.map(_.toFile)
              ).toSet

          }

          if (jsonFiles == null || jsonFiles.isEmpty)
            log.info(s"found no JSON-schema files for generating code")

          cachedFun(jsonFiles.toSet).toSeq
      }

  )

  lazy override val projectSettings: Seq[Def.Setting[_]] = jsonGenSettings(Compile)

}