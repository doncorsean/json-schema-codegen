package json.schema.codegen

import json.schema.parser.JsonSchemaParser
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{Success, Validation}

class ScalaModelGeneratorTest extends FlatSpec  with Matchers  {


  def parse(s: String): Validation[String, ScalaType] = JsonSchemaParser.parse(s).validation.flatMap(ScalaModelGenerator(_)).map(_.head)
  def parseAll(s: String): Validation[String, Set[ScalaType]] = JsonSchemaParser.parse(s).validation.flatMap(ScalaModelGenerator(_))

  ScalaModelGenerator.getClass.getName should "convert simple types to Scala types" in {
    parse(
      """
        |{"type":"integer"}
      """.stripMargin).map(_.identifier) shouldBe Success("Int")
    parse(
      """
        |{"type":"boolean"}
      """.stripMargin).map(_.identifier) shouldBe Success("Boolean")
    parse(
      """
        |{"type":"number"}
      """.stripMargin).map(_.identifier) shouldBe Success("Double")
    parse(
      """
        |{"type":"integer"}
      """.stripMargin).map(_.identifier) shouldBe Success("Int")
    parse(
      """
        |{"type":"string"}
      """.stripMargin).map(_.identifier) shouldBe Success("String")
  }

  it should "convert simple types with format to Scala types" in {
    parse(
      """
        |{"type":"string",
        |"format":"uri"
        |}
      """.stripMargin).map(_.identifier) shouldBe Success("URI")
  }

  it should "convert array of unique items to Scala Set" in {
    parse(
      """
        |{"type":"array",
        |"items":{"type":"string"}, "uniqueItems":true
        |}
      """.stripMargin).map(_.toString) shouldBe Success("Set[String]")
  }

  it should "convert array of items to Scala List" in {
    parse(
      """
        |{"type":"array",
        |"items":{"type":"string"}
        |}
      """.stripMargin).map(_.toString) shouldBe Success("List[String]")
  }

  it should "use id in camel case for class name" in {
    parse(
      """
        |{
        | "id": "http://some/product",
        |"type":"object"
        |}
      """.stripMargin).map(_.toString) shouldBe Success("Product")
    parse(
      """
        |{
        | "id": "http://some/path#/product",
        |"type":"object"
        |}
      """.stripMargin).map(_.toString) shouldBe Success("path.Product")
  }

  it should "create type with members from properties" in {
    parse(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"string"},
        |"b":{"type":"number"}
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.toString))) shouldBe Success(
      List(
        ("a", "String"),
        ("b", "Option[Double]")
      )
    )
  }

  it should "create type using definitions" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{"type":"string"}
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.toString, p.isa.scope))) shouldBe Success(
      List(
        ("a", "String", "")
      )
    )
  }

  it should "create type with name using explicit id " in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "id":"#/definitions/typea",
        | "type":"object",
        | "properties":{
        | "b":{"type":"string"}
        | }
        | }
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.toString))) shouldBe Success(
      List(
        ("a", "product.definitions.Typea")
      )
    )
  }

  it should "create type with field name as type name, when id is not specified" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "type":"object",
        | "properties":{
        | "nested":{"type":"string"}
        | }
        | }
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.toString))) shouldBe Success(
      List(
        ("a", "product.definitions.Typea")
      )
    )
  }

  it should "create type reusing same sub type" in {
    parse(
      """
        |{
        |"id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"},
        |"b":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "type":"object",
        | "properties":{
        | "nested":{"type":"string"}
        | }
        | }
        |},
        |"required":["a","b"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.toString))) shouldBe Success(
      List(
        ("a", "product.definitions.Typea"),
        ("b", "product.definitions.Typea")
      )
    )
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"},
        |"b":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "id":"#/definitions/typea",
        | "type":"object",
        | "properties":{
        | "b":{"type":"string"}
        | }
        | }
        |},
        |"required":["a","b"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.toString))) shouldBe Success(
      List(
        ("a", "product.definitions.Typea"),
        ("b", "product.definitions.Typea")
      )
    )
  }
  it should "create enum type " in {
    parse(
      """
        |{
        |"type":"string",
        |"enum": ["a","b"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaEnum].enums) shouldBe Success(
      Set(
        "a", "b"
      )
    )
    parse(
      """
        |{
        |"type":"number",
        |"enum": [1,2]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaEnum].enums) shouldBe Success(
      Set(
        1d,2d
      )
    )
  }


  it should "type with additionalProperties has a map" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"additionalProperties":{"$ref": "#/definitions/typea"},
        |"definitions":{
        | "typea":{
        | "id":"#/definitions/typea",
        | "type":"object",
        | "properties":{
        | "nested":{"type":"string"}
        | }
        | }
        |}
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].additionalNested.map(_.identifier)) shouldBe Success(
      Some(
        "Typea"
      )
    )
  }

  it should "preserve scope of referenced types" in {
    val models: Validation[String, Set[ScalaType]] =parseAll(
      """
        |{
        |"id": "product",
        |"type":"object",
        |"properties":{
        | "typea":{
        |   "$ref":"nested-schema-entity"
        |  }
        | },
        | "schema2": {
        |   "id": "nested-schema-entity",
        |   "type":"object"
        | }
        |}
      """.stripMargin)
    models.map(_.size) shouldBe Success(2)
    models.map(_.find(_.identifier =="Product").map(_.scope)) shouldBe Success(Some(""))
    models.map(_.find(_.identifier =="Entity").map(_.scope)) shouldBe Success(Some("nested.schema"))
  }

}
