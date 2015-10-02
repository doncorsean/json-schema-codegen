package json.schema

import java.nio.file.Path

import json.schema.parser.{JsonSchemaParser, SchemaDocument}
import json.source.JsonSource

import scalaz.Leibniz
import scalaz.Leibniz._
import scalaz.Scalaz._

package object codegen {

  type SValidation[T] = scalaz.Validation[String, T]

  sealed trait LangType {
    // types referenced by this type
    val referenced: Set[LangType]
    // name space of this type
    val scope: String
    // language specific valid identifier
    val identifier: String
  }

  sealed case class PredefType(scope: String, identifier: String) extends LangType {
    override val referenced: Set[LangType] = Set.empty
  }

  sealed case class ArrayType(scope: String, unique: Boolean, nested: LangType) extends LangType {
    override val identifier = nested.identifier
    override lazy val referenced: Set[LangType] = nested.referenced + nested
  }

  sealed case class ClassType(scope: String, identifier: String, properties: List[LangTypeProperty], additionalNested: Option[LangType]) extends LangType {
    override lazy val referenced: Set[LangType] = properties.map(t => t.isa.referenced + t.isa).toSet.flatten ++ additionalNested.map(t => t.referenced + t).getOrElse(Set.empty)
  }

  sealed case class EnumType(scope: String, identifier: String, nested: LangType, enums: Set[_]) extends LangType {
    override lazy val referenced: Set[LangType] = nested.referenced + nested
  }

  case class LangTypeProperty(name: String, required: Boolean, isa: LangType)


  implicit class ParserWrapper[N: Numeric, T: JsonSource](jsonParser: JsonSchemaParser[N]) {
    implicit val evdoc: ===[SValidation[SchemaDocument[N]], SValidation[SchemaDocument[N]]] = Leibniz.refl

    def parseAll(sources: Seq[T]): SValidation[List[SchemaDocument[N]]] = sources.map(source => jsonParser.parse(source).validation).toList.sequenceU
  }


  trait CodeGen {
    def apply[N: Numeric](schemas: List[SchemaDocument[N]])(codeGenTarget: Path): SValidation[List[Path]]
  }

}
