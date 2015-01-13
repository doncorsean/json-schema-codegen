package json.schema.codegen

import java.io.File
import java.nio.file.Path

import json.schema.parser.{SchemaDocument, JsonSchemaParser}
import json.source.JsonSource

import scalaz.Scalaz._


object CodeGenerator extends CodeGen {

  private implicit class Printable[T](v: T) {
    def pp(prefix: String = ""): T = {
      println(s"$prefix : $v")
      v
    }
  }

  def gen[N: Numeric, T: JsonSource](jsonParser: JsonSchemaParser[N], source: T)(codeGenTarget: Path) = {

    for {
      schema: SchemaDocument[N] <- jsonParser.parse(source).validation.pp("parsed schema")
      models: Set[ScalaType] <- ScalaModelGenerator(schema).pp("generated object model")
      modelFiles: Seq[Path] <- generateModel(models, schema.scope, codeGenTarget).pp("model files")
      codecFiles: Seq[Path] <- generateCodec(models, schema.scope, codeGenTarget).pp("serializatoin files")
    } yield (modelFiles ++ codecFiles).pp("all generated files")

  }

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
      println(s"Code generation failed with: $e")
      System.exit(1)
    }
    ,
    _ => System.exit(0)
    )

  }
}
