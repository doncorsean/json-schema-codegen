package json.schema.codegen

import java.io.File
import java.nio.file.Path

import json.schema.parser.{SchemaDocument, JsonSchemaParser}
import json.source.JsonSource

import scalaz.Scalaz._


object CodeGenerator extends CodeGen {

  def main(args: Array[String]) {

    val oargs = args.lift

    val result = for {
      source <- oargs(0).map(new File(_)).toSuccess("json-schema is required")
      targetDir <- oargs(1).map(new File(_)).toSuccess("target folder is required")
      genRoot: Path = targetDir.toPath
      generator = if (true) gen(JsonSchemaParser, source)(_) else gen(new JsonSchemaParser[Float], source)(_)
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