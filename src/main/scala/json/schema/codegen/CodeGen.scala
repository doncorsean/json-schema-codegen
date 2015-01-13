package json.schema.codegen

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{StandardOpenOption, OpenOption, Files, Path}

import scala.util.control.NonFatal
import scalaz.Scalaz._
import scalaz.Validation

object CodeGen {

  implicit class StringTools(v: Option[String]) {
    def noneIfEmpty: Option[String] = v match {
      case Some(s) if s.isEmpty => none
      case _ => v
    }
  }

}

trait CodeGen extends Naming {

  import CodeGen._

  val additionalPropertiesMember = "_additional"

  def generateFile(scope: URI, fileName: String, outputDir: Path)(content: String => Validation[String, String]): Validation[String, Seq[Path]] = {

    val packageName = genPackageName(scope)

    try {

      val packageDir: String = packageName.replaceAll("\\.", File.separator)
      val generatedPackageFile = packageDir + File.separator + fileName

      // create package structure
      Files.createDirectories(outputDir.resolve(packageDir))
      content(packageName) map {
        fileContent =>
          val generateAbsoluteFile: Path = outputDir.resolve(generatedPackageFile)
          Files.deleteIfExists(generateAbsoluteFile)
          Seq(
            Files.write(generateAbsoluteFile, fileContent.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE_NEW)
          )
      }

    } catch {
      case NonFatal(e) => e.toString.failure
    }

  }

  def generateCodec(ts: Iterable[ScalaType], scope: URI, outputDir: Path): Validation[String, Seq[Path]] = {
    generateFile(scope, "Codecs.scala", outputDir) {
      packageName =>

        val codecs = ts.map {
          case t: ScalaClass => genCodec(t)
          case _ => ""
        }.mkString("\n")

        s"""
         |package $packageName
         |
         |import argonaut._, Argonaut._
         |
         |object Codecs {
         |$codecs
         |}
        """.stripMargin.success

    }

  }

  def generateModel(ts: Iterable[ScalaType], scope: URI, outputDir: Path): Validation[String, Seq[Path]] = {
    generateFile(scope, "model.scala", outputDir) {
      packageName =>

        ts.map {
          case t: ScalaClass => genType(t)
          case _ => ""
        }.mkString(
            s"package $packageName\n\n",
            "\n\n", ""
          ).success

    }
  }

  def genCodec(c: ScalaClass): String = {
    val propNames = c.properties.map(p => '"' + p.name + '"').mkString(", ")
    val className = c.identifier
    c.additionalNested.fold(
      s"""
       |  implicit def ${className}Codec: CodecJson[$className]=casecodec${c.properties.length}($className.apply, $className.unapply)($propNames)
     """.stripMargin
    )(
        additionalType =>
          s"""
           |  implicit def ${className}Codec: CodecJson[$className]=CodecJson(MapEncodeJson[$className], MapDecodeJson[$className])
         """.stripMargin
      )
  }

  def genPackageName(scope: URI) = {
    val simpleScope = scope.getFragment.some.noneIfEmpty orElse scope.getPath.some.noneIfEmpty.map(p => new File(p).getName) orElse scope.getHost.some.noneIfEmpty getOrElse "local"
    simpleScope.map(c => c.isLetterOrDigit ? c | '.').replaceAll("\\.+$", "").replaceAll("^\\.+", "")
  }

  def genPropertyType(t: ScalaType): String = {
    t match {
      case a: ScalaArray => if (a.unique) s"Set[${a.nested.identifier}]" else s"List[${a.nested.identifier}]"
      case a: ScalaType => a.identifier
    }
  }

  def genPropertyType(p: ScalaTypeProperty): String = {
    val t = genPropertyType(p.isa)
    p.required ? t | s"Option[$t]"
  }

  def genType(clazz: ScalaType): String = clazz match {
    case t: ScalaClass =>
      val properties = t.properties.map {
        p =>
          val propType = genPropertyType(p)
          val member = underscoreToCamel(identifier(p.name))
          s"$member:$propType"
      }
      val extra = t.additionalNested.map {
        tn =>
          val propType = genPropertyType(tn)
          s"$additionalPropertiesMember:Map[String, $propType]"
      }
      val members = (properties ++ extra.toList).mkString(", ")
      s"""case class ${t.identifier}($members)""".stripMargin

    // enum of number are not support
    case t: ScalaEnum if t.nested.identifier == "Double" => ""
    case t: ScalaEnum =>
      val valueDeclarations = t.enums.map {
        case v: String =>
          val valueId = identifier(v)
          s"""val $valueId = Value("$v")"""
        case v: Int =>
          val valueId = s"v${v.toInt}"
          s"val $valueId = Value(${v.toInt})"
        case v: Double =>
          val valueId = s"v${v.toInt}"
          s"val $valueId = Value(${v.toInt})"
        case _ => ""
      }.filter(_ != "").mkString("\n")
      s"""object ${t.identifier} extends Enumeration {
         |$valueDeclarations
         |}""".stripMargin

    case _ => ""

  }

}


