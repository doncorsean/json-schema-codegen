package json.schema.codegen

import argonaut.Json
import json.schema.parser.{SchemaDocument, SimpleType}
import scalaz.Scalaz._

class ModelGenerator[N: Numeric](json2predef: Map[SimpleType.SimpleType, PredefType], format2predef: Map[(PredefType, String), PredefType]) extends Naming {

  type Schema = SchemaDocument[N]

  val definedSchemas = scala.collection.mutable.Map.empty[Schema, LangType]

  def `object`(schema: Schema, name: Option[String]): SValidation[LangType] = {

    schema.obj.toSuccess(s"not object type: ${schema.types}").flatMap {
      obj =>
        val schemaClassName: SValidation[String] = className(schema, name)

        val propertyTypes: List[SValidation[LangTypeProperty]] = obj.properties.value.map {
          case (propName, propDefinition) =>

            val existingType = definedSchemas.get(propDefinition.schema).toSuccess("no type")

            val propDef = existingType orElse any(propDefinition.schema, propName.some) map {
              t =>
                LangTypeProperty(propName, propDefinition.required, t)
            }

            scalaz.Validation.fromEither(propDef.toEither.leftMap(e => s"Type for field ${schemaClassName.toOption}.$propName not found: $e"))

        }.toList

        val propTypes: SValidation[List[LangTypeProperty]] = propertyTypes.sequence

        for {
          props <- propTypes
          className <- schemaClassName
          additional <- obj.additionalProperties.toList.map(nested => any(nested, (className + "Additional").some))
            .sequence.map(_.headOption)
        } yield {
          val newType = ClassType(packageName(schema.id.getOrElse(schema.scope)), className, props, additional)
          definedSchemas.put(schema, newType)
          newType.asInstanceOf[LangType]
        }
    }
  }


  def array(schema: Schema, name: Option[String]): SValidation[LangType] =
    schema.array.toSuccess(s"not array type: ${schema.types}").flatMap {
      array =>
        val genClassName: Option[String] = name.map(_ + "0")
        val arrayDef = any(array.items.value.head, genClassName) map {
          nested =>
            ArrayType(packageName(schema.id.getOrElse(schema.scope)), array.uniqueItems, nested)
        }

        scalaz.Validation.fromEither(arrayDef.toEither.leftMap(e => s"Type of Array $genClassName not found: $e"))
    }

  def simple(schema: Schema): SValidation[LangType] = {
    schema.types.headOption.flatMap(json2predef.get).toSuccess("Type is not simple") map {
      simpleType =>
        // if there is a format, try to find type that corresponds to the format
        val formatType = schema.format.flatMap(format => format2predef.get((simpleType, format.toLowerCase))).getOrElse(simpleType)

        definedSchemas.put(schema, formatType)
        formatType
    }
  }


  def enum(schema: Schema, name: Option[String]): SValidation[LangType] = {

    for {
      t <- schema.types.headOption.toSuccess("Type is required")
      className <- className(schema, name)
      enums: Set[Json] <- schema.enums.isEmpty ? "Enum not defined".fail[Set[Json]] | schema.enums.success[String]
      enumNestedSchema = schema.copy(enums = Set.empty)
      nestedType <- any(enumNestedSchema, (className + "Value").some)
    } yield {

      val enumNested = t match {
        case SimpleType.string => enums.flatMap(_.string.toSeq)
        case SimpleType.number => enums.flatMap(_.number.toSeq)
        case SimpleType.integer => enums.flatMap(_.number.toSeq)
        case SimpleType.boolean => enums.flatMap(_.bool.toSeq)
      }

      val newType = EnumType(packageName(schema.scope), className, nestedType, enumNested)
      definedSchemas.put(schema, newType)
      newType
    }

  }


  def any(schema: Schema, name: Option[String]): SValidation[LangType] = {
    if (schema.types.length != 1)
      s"One type is required in: $schema".fail
    else
      enum(schema, name) orElse array(schema, name) orElse `object`(schema, name) orElse simple(schema)
  }


}
