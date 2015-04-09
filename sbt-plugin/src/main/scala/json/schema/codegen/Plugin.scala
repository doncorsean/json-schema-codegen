package json.schema.codegen

import java.io.{FilenameFilter, File}
import java.nio.file.Path

import json.schema.parser.JsonSchemaParser
import sbt.Keys._
import sbt.{Def, _}

object Plugin extends sbt.AutoPlugin {

  override def trigger = noTrigger

  object autoImport {
    lazy val jsonCodegen = TaskKey[Seq[File]]("json-schema-codegen", "Generate code from Json-Schema")
  }

  import autoImport._

  lazy val jsonGenSettings: Seq[Def.Setting[_]] = Seq(

    sourceDirectory in jsonCodegen <<= sourceDirectory(src => src / "json-schema"),

    sources in jsonCodegen <<= (sourceDirectory in jsonCodegen) map {
      (schemaSrc) =>
        (schemaSrc ** GlobFilter("*.json")).get
    },

    jsonCodegen <<= (sourceManaged, streams, sources in jsonCodegen) map {
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
    },

    watchSources in Defaults.ConfigGlobal <++= sources in jsonCodegen,

    sourceGenerators <+= jsonCodegen

  )

  lazy override val projectSettings: Seq[Def.Setting[_]] = inConfig(Compile)(jsonGenSettings)

}