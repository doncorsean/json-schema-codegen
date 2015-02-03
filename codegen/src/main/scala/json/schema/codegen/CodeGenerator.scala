package json.schema.codegen

import java.io.{FilenameFilter, File}
import java.nio.file.Path

import json.schema.parser.JsonSchemaParser

import scalaz.syntax.std.all._

object CodeGenerator extends CodeGen {

  def main(args: Array[String]) {

    val oargs = args.lift

    val result = for {
      source <- oargs(0).map(new File(_)).toSuccess("json-schema is required")
      targetDir <- oargs(1).map(new File(_)).toSuccess("target folder is required")
      genRoot: Path = targetDir.toPath
      sources: Seq[File] = if (source.isDirectory) source.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
      })
      else Seq(source)
      generator = if (true) gen(JsonSchemaParser, sources)(_) else gen(new JsonSchemaParser[Float], sources)(_)
      results <- generator(genRoot)
    } yield results

    result.fold(
    { e =>
      error(s"Code generation failed with: $e")
      System.exit(1)
    }
    ,
    _ => System.exit(0)
    )

  }

}