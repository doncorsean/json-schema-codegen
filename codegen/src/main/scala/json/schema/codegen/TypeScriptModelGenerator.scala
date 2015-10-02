package json.schema.codegen

import json.schema.parser.{SchemaDocument, SimpleType}

import scalaz.Scalaz._


object TypeScriptModelGenerator {

  private val preDefScope = ""

  val format2ts: Map[(PredefType, String), PredefType] = Map(
    (PredefType(preDefScope, "String"), "uri") -> PredefType(preDefScope, "string"),
    (PredefType(preDefScope, "String"), "date-time") -> PredefType(preDefScope, "Date"),
    (PredefType(preDefScope, "String"), "ipv6") -> PredefType("java.net", "string"),
    (PredefType(preDefScope, "String"), "ipv4") -> PredefType("java.net", "string"),
    (PredefType(preDefScope, "String"), "email") -> PredefType(preDefScope, "string"),
    (PredefType(preDefScope, "String"), "hostname") -> PredefType(preDefScope, "string")
  )

  val json2ts: Map[SimpleType.SimpleType, PredefType] = Map(
    SimpleType.string -> PredefType(preDefScope, "string"),
    SimpleType.integer -> PredefType(preDefScope, "number"),
    SimpleType.boolean -> PredefType(preDefScope, "boolean"),
    // same as schema's document type param
    SimpleType.number -> PredefType(preDefScope, "number"),
    SimpleType.`null` -> PredefType(preDefScope, "any")
  )

  def apply[N: Numeric](schema: SchemaDocument[N]): scalaz.Validation[String, Set[LangType]] = {

    val generator: ModelGenerator[N] = new ModelGenerator[N](json2ts, format2ts)

    val typeName = generator.className(schema.scope).some

    generator.any(schema, typeName) map {
      t =>
        (t :: generator.definedSchemas.values.toList).toSet // to remove duplicate types
    }
  }

}