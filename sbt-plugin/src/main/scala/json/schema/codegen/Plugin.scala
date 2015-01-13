package json.schema.codegen

import java.io.File
import java.nio.file.Path

import json.schema.parser.JsonSchemaParser
import sbt.Keys._
import sbt.{Def, _}

object Plugin extends sbt.AutoPlugin {


  object Keys {
    val jsonSchemaSourceFolder = settingKey[File]("folder for the Json-Schema")
  }

  private val jsonCodegen = TaskKey[Seq[File]]("json-schema-codegen", "Generate code from Json-Schema")

  override def projectSettings: Seq[Def.Setting[_]] = Seq(

    Keys.jsonSchemaSourceFolder <<= (sourceDirectory in Compile)(src => src / "json-schema"),

    sourceGenerators in Compile <+= (jsonCodegen in Compile),

    jsonCodegen in Compile <<=
      (sourceManaged in Compile, streams, Keys.jsonSchemaSourceFolder) map {
        (dir, s, schemaSrc) =>

          val log = s.log("json-schema-codegen")

          val cachedFun = FileFunction.cached(s.cacheDirectory / "json-schema",
            FilesInfo.lastModified, /* inStyle */
            FilesInfo.exists) /* outStyle */ {
            (in: Set[File]) =>

              in.flatMap {
                source =>
                  val genRoot: Path = dir.toPath
                  log.info(s"Generating code using $source in $genRoot")
                  val generator = if (true) CodeGenerator.gen(JsonSchemaParser, source)(_) else CodeGenerator.gen(new JsonSchemaParser[Float], source)(_)
                  generator(genRoot).fold(
                    e => throw new IllegalArgumentException(s"Failed code generation in $source: $e "),
                    p => p.map(_.toFile)
                  ).toSet
              }
          }

          // track all source folders under dart, except "build", since it is modified for each build
          val srcFolders = schemaSrc.listFiles()
          if (srcFolders == null)
            throw new IllegalArgumentException(s"no schema files found in $schemaSrc")

          cachedFun(srcFolders.toSet).toSeq
      }

  )

}