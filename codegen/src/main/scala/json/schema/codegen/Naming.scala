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

   def dotNotation(scope: URI) = {

    val fragment: String = scope.getFragment.some.noneIfEmpty.map(s => s.startsWith("/") ? s | "/" + s).getOrElse("")
    // package from URI's fragment, path or host
    lazy val fromURI: String = scope.getPath.some.noneIfEmpty.getOrElse("") + fragment

    // package from file URI , using only the file name
    val simpleScope: String = try {
      (scope.getScheme == "file") ? (new File(new URI(scope.getScheme, scope.getHost, scope.getPath, null)).getName + fragment) | fromURI
    } catch {
      case NonFatal(e) => fromURI
    }

    val dottedString = removeExtension(simpleScope).map(c => Character.isJavaIdentifierPart(c) ? c | '.').replaceAll("\\.+$", "").replaceAll("^\\.+", "")

    dottedString.split('.').map( s=> escapeKeyword(underscoreToCamel(identifier(s))) )
  }

  def packageName(scope: URI): String = {
    val dots = dotNotation(scope)
    dots.take(dots.size - 1).mkString(".")
  }

  def className(scope: URI): String = {
    val dots = dotNotation(scope)
    val name = dots.lastOption.getOrElse(dots.head)
    escapeKeyword(underscoreToCamel(identifier(name))).capitalize
  }

  def className(schema: SchemaDocument[_], defaultName: Option[String]): scalaz.Validation[String, String] = (
    schema.id.toSuccess("Schema has no Id").flatMap(identifier).map(underscoreToCamel) orElse defaultName.toSuccess("Default name not given").map(s => underscoreToCamel(identifier(s)))
    ).map(s => escapeKeyword(s).capitalize)

  def memberName(s: String) = escapeKeyword(underscoreToCamel(identifier(s)))

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

  def escapeKeyword(s: String) = keywords.contains(s) ? ('_' + s) | s

  val keywords = Set(
    "abstract", "case", "catch", "class",
    "def", "do", "else", "extends",
    "false", "final", "finally", "for",
    "forSome", "if", "implicit", "import",
    "lazy", "match", "new", "null",
    "object", "override", "package", "private",
    "protected", "return", "sealed", "super",
    "this", "throw", "trait", "try",
    "true", "type", "val", "var",
    "while", "with", "yield"
  )
}
