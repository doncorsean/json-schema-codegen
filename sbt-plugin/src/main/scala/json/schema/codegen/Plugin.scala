package json.schema.codegen

import java.io.File

import json.schema.parser.JsonSchemaParser
import sbt.Keys._
import sbt.{Def, _}

object Plugin extends sbt.AutoPlugin {

  override def trigger = noTrigger

  object autoImport {
    lazy val typeScriptDirectory = SettingKey[Option[File]]("type-script-directory", "Destination Folder for TypeScript generated code")
    lazy val scalaCodegen = TaskKey[Seq[File]]("scala-codegen", "Generate Scala code from Json-Schema")
    lazy val typescriptCodegen = TaskKey[Seq[File]]("typescript-codegen", "Generate TypeScript code from Json-Schema")
  }

  import autoImport._

  lazy val jsonGenSettings: Seq[Def.Setting[_]] = Seq(

    // TypeScript generation is disabled by default.
    typeScriptDirectory := None,

    // scala code gen
    sourceDirectory in scalaCodegen <<= sourceDirectory(src => src / "json-schema"),
    sources in scalaCodegen <<= (sourceDirectory in scalaCodegen) map {
      (schemaSrc) =>
        (schemaSrc ** GlobFilter("*.json")).get
    },
    watchSources in Defaults.ConfigGlobal <++= sources in scalaCodegen,
    scalaCodegen <<= (sourceManaged, streams, sources in scalaCodegen) map {
      (destinationDir, s, jsonFiles: Types.Id[Seq[sbt.File]]) =>
        runGen(s, destinationDir, new ScalaGenerator with SbtLog {
          val log = s.log("scala-codegen")
        }, jsonFiles)
    },
    sourceGenerators <+= scalaCodegen,

    // typescript gen
    sourceDirectory in typescriptCodegen <<= sourceDirectory in typescriptCodegen,
    sources in typescriptCodegen <<= (sourceDirectory in typescriptCodegen) map {
      (schemaSrc) =>
        (schemaSrc ** GlobFilter("*.json")).get
    },
    typescriptCodegen <<= (typeScriptDirectory, streams, sources in typescriptCodegen) map {
      (destinationDirOpt, s, jsonFiles: Types.Id[Seq[sbt.File]]) =>
        destinationDirOpt.foreach(destinationDir =>
          runGen(s, destinationDir, new TypeScriptGenerator with SbtLog {
            val log = s.log("typescript-codegen")
          }, jsonFiles)
        )
        Nil
    },
    resourceGenerators <+= typescriptCodegen

  )


  trait SbtLog extends Logging {
    def log: sbt.Logger

    override def debug(s: => String): Unit = log.debug(s)

    override def info(s: => String): Unit = log.info(s)

    override def error(s: => String): Unit = log.error(s)
  }


  def runGen(s: TaskStreams, destinationDir: File, generator: CodeGenerator, jsonFiles: Seq[sbt.File]) = {

    val cachedFun = FileFunction.cached(s.cacheDirectory / "json-schema",
      FilesInfo.lastModified, /* inStyle */
      FilesInfo.exists) /* outStyle */ {
      (jsonSchemas: Set[File]) =>

        val destinationPath = destinationDir.toPath


        generator.info(s"${generator.getClass} generating code using $jsonSchemas in $destinationPath")

        val genFiles = for {
          schemas <- JsonSchemaParser.parseAll(jsonSchemas.toSeq)
          result <- generator(schemas)(destinationPath)
        } yield result


        genFiles.fold(
          e => throw new IllegalArgumentException(s"Failed code generation in ${jsonSchemas.map(s=>s.toString.split("/").last).mkString(",")}\nException: $e "),
          p => p.map(_.toFile)
        ).toSet

    }

    if (jsonFiles == null || jsonFiles.isEmpty)
      generator.info(s"found no JSON-schema files for generating code")

    cachedFun(jsonFiles.toSet).toSeq
  }


  lazy override val projectSettings: Seq[Def.Setting[_]] = inConfig(Compile)(jsonGenSettings)

}