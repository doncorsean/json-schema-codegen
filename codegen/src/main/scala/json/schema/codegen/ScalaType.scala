package json.schema.codegen

import scalaz.Scalaz._

case class ScalaTypeProperty(name: String, required: Boolean, isa: ScalaType) {
  override def toString = required ? isa.toString | s"Option[$isa]"
}

sealed trait ScalaType {
  val referenced: Set[ScalaType]
  val scope: String
  val identifier: String
  override def toString = scope.isEmpty ? identifier | (scope + "." + identifier)
}

sealed case class ScalaSimple(scope: String, identifier: String) extends ScalaType {
  override val referenced: Set[ScalaType] = Set.empty
}

sealed case class ScalaArray(scope: String, unique: Boolean, nested: ScalaType) extends ScalaType {
  override val identifier  = nested.identifier
  override lazy val referenced: Set[ScalaType] = nested.referenced + nested
  override def toString = if (unique) s"Set[$nested]" else s"List[$nested]"

}

sealed case class ScalaClass(scope: String, identifier: String, properties: List[ScalaTypeProperty], additionalNested: Option[ScalaType]) extends ScalaType {
  override lazy val referenced: Set[ScalaType] = properties.map( t => t.isa.referenced + t.isa).toSet.flatten ++ additionalNested.map( t => t.referenced + t).getOrElse(Set.empty)
}

sealed case class ScalaEnum(scope: String, identifier: String, nested: ScalaType, enums: Set[_]) extends ScalaType {
  override lazy val referenced: Set[ScalaType] =  nested.referenced + nested
}




