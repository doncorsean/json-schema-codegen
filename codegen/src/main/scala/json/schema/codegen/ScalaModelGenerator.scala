package json.schema.codegen

import argonaut.Json
import json.schema.parser.SimpleType._
import json.schema.parser.{SchemaDocument, SimpleType}

import scala.collection.mutable
import scalaz.Leibniz
import scalaz.Scalaz._

private class ScalaModelGenerator[N](implicit numeric: Numeric[N]) extends Naming {
  type Schema = SchemaDocument[N]

  import json.schema.codegen.ScalaModelGenerator._

  val types = mutable.Map.empty[Schema, ScalaType]

  val json2scala: Map[SimpleType, ScalaSimple] = Map(
    SimpleType.string -> ScalaSimple("String"),
    SimpleType.integer -> ScalaSimple("Long"),
    SimpleType.boolean -> ScalaSimple("Boolean"),
    // same as schema's document type param
    SimpleType.number -> ScalaSimple(numeric.zero.getClass.getSimpleName),
    SimpleType.`null` -> ScalaSimple("Any")
  )

  val format2scala: Map[(ScalaSimple, String), ScalaSimple] = Map(
    (ScalaSimple("String"), "uri") -> ScalaSimple("java.net.URI"),
    (ScalaSimple("String"), "date-time") -> ScalaSimple("java.util.Date"),
    (ScalaSimple("String"), "ipv6") -> ScalaSimple("java.net.Inet6Address"),
    (ScalaSimple("String"), "ipv4") -> ScalaSimple("java.net.Inet4Address"),
    (ScalaSimple("String"), "email") -> ScalaSimple("String"),
    (ScalaSimple("String"), "hostname") -> ScalaSimple("String")
  )

  def `object`(schema: Schema, name: Option[String]): Validation[ScalaType] = {

    implicit val ev = Leibniz.refl[Validation[ScalaTypeProperty]]

    if (SimpleType.`object` == schema.common.types.headOption.orNull) {

      val schemaClassName: Validation[String] = className(schema, name)

      val propertyTypes: List[Validation[ScalaTypeProperty]] = schema.properties.value.map {
        case (n, prop) =>

          val existingType = types.get(prop.schema).toSuccess("no type")

          val propDef = existingType orElse any(prop.schema, n.some) map {
            t =>
              ScalaTypeProperty(n, prop.required, t)
          }

          scalaz.Validation.fromEither(propDef.toEither.leftMap(e => s"Type for field ${schemaClassName.toOption}.$n not found: $e"))

      }.toList

      for {
        props <- propertyTypes.sequence
        className <- schemaClassName
        additional <- schema.additionalProperties.toList.map(nested => any(nested, (className + "Additional").some)).sequence.map(_.headOption)
      } yield {
        val newType = ScalaClass(className, props, additional)
        types.put(schema, newType)
        newType
      }

    } else {
      s"not object type: ${schema.common.types}".fail
    }
  }


  def array(schema: Schema, name: Option[String]): Validation[ScalaType] = {
    if (SimpleType.array == schema.common.types.headOption.orNull) {
      val genClassName: Option[String] = name.map(_ + "0")
      val arrayDef = any(schema.items.value.head, genClassName) map {
        nested =>
          ScalaArray(schema.uniqueItems, nested)
      }

      scalaz.Validation.fromEither(arrayDef.toEither.leftMap(e => s"Type of Array $genClassName not found: $e"))

    } else {
      s"not array type: ${schema.common.types}".fail
    }
  }

  def simple(schema: Schema): Validation[ScalaType] = {
    schema.common.types.headOption.flatMap(json2scala.get).toSuccess("Type is not simple") map {
      simpleType =>
        // if there is a format, try to find type that corresponds to the format
        val formatType = schema.common.format.flatMap(format =>
          format2scala.get((simpleType, format.toLowerCase))
        ).getOrElse(simpleType)

        types.put(schema, formatType)
        formatType
    }
  }


  def enum(schema: Schema, name: Option[String]): Validation[ScalaType] = {

    for {
      t <- schema.common.types.headOption.toSuccess("Type is required")
      className <- className(schema, name)
      enums: Set[Json] <- schema.enums.isEmpty ? "Enum not defined".fail[Set[Json]] | schema.enums.success[String]
      enumNestedSchema = schema.copy(enums = Set.empty)
      nestedType <- any(enumNestedSchema, (className + "Value").some)
    } yield {

      val enumNested = t match {
        case SimpleType.string => enums.map(_.string.toSeq).flatten
        case SimpleType.number => enums.map(_.number.toSeq).flatten
        case SimpleType.integer => enums.map(_.number.toSeq).flatten
        case SimpleType.boolean => enums.map(_.bool.toSeq).flatten
      }

      val newType = ScalaEnum(className, nestedType, enumNested)
      types.put(schema, newType)
      newType
    }

  }


  def any(schema: Schema, name: Option[String]): Validation[ScalaType] = {
    if (schema.common.types.length != 1)
      s"One type is required in: $schema".fail
    else
      enum(schema, name) orElse array(schema, name) orElse `object`(schema, name) orElse simple(schema)
  }

}

object ScalaModelGenerator {

  type Validation[T] = scalaz.Validation[String, T]

  def apply[N: Numeric](schema: SchemaDocument[N]): scalaz.Validation[String, Set[ScalaType]] = {

    val generator: ScalaModelGenerator[N] = new ScalaModelGenerator()

    val typeName = generator.className(schema.scope).some

    generator.any(schema, typeName) map {
      t =>
        (t :: generator.types.values.toList).toSet // to remove duplicate types
    }
  }

}