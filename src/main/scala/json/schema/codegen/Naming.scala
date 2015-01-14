package json.schema.codegen

import java.io.File
import java.net.URI
import json.schema.parser.SchemaDocument

import scala.util.control.NonFatal
import scalaz.Scalaz._

trait Naming {

  implicit class StringToolsO(v: Option[String]) {
    def noneIfEmpty: Option[String] = v match {
      case Some(s) if s == null || s.isEmpty => none
      case _ => v
    }
  }

  def removeExtension(s: String) = {
    val extIndex = s.lastIndexOf('.')
    (extIndex >= 0) ? s.substring(0, extIndex) | s
  }

  def packageName(scope: URI): Option[String] = {
    // package from URI's fragment, path or host
    lazy val fromURI = scope.getFragment.some.noneIfEmpty orElse scope.getPath.some.noneIfEmpty orElse scope.getHost.some.noneIfEmpty

    // package from file URI , using only the file name
    val simpleScope = try {
      (scope.getScheme == "file") ? new File(scope.getPath).getName.some | fromURI
    } catch {
      case NonFatal(e) => fromURI
    }
    simpleScope.map(removeExtension).map(_.map(c => c.isLetterOrDigit ? c | '.').replaceAll("\\.+$", "").replaceAll("^\\.+", ""))
  }

  def className(scope: URI): scalaz.Validation[String, String] = identifier(scope).map(underscoreToCamel).map(_.capitalize)

  def className(schema: SchemaDocument[_], defaultName: Option[String]): scalaz.Validation[String, String] = (
    schema.id.toSuccess("Schema has no Id").flatMap(identifier).map(underscoreToCamel) orElse defaultName.toSuccess("Default name not given").map(s => underscoreToCamel(identifier(s)))
    ).map(_.capitalize)

  def identifier(scope: URI): scalaz.Validation[String, String] = {
    val str = scope.toString
    val lastSlash: Int = str.lastIndexOf('/')
    val lastSegment = (lastSlash >= 0) ? str.substring(lastSlash) | str
    val noExtSegment = removeExtension(lastSegment)
    identifier(noExtSegment.filter(c => c != '#')).some.noneIfEmpty.toSuccess(s"Unable to extract identifier from $scope")
  }

  def identifier(s: String): String = s.map(c => c.isLetterOrDigit ? c | '_')

  def underscoreToCamel(name: String): String = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    m.group(1).toUpperCase
  })

}
