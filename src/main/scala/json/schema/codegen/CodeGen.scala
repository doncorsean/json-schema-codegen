package json.schema.codegen

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import json.schema.parser.{JsonSchemaParser, SchemaDocument}
import json.source.JsonSource

import scala.util.control.NonFatal
import scalaz.Scalaz._
import scalaz.Validation

trait CodeGen extends Naming {

  val addPropName = "_additional"

  def generateFile(scope: URI, fileName: String, outputDir: Path)(content: Option[String] => Validation[String, String]): Validation[String, Seq[Path]] = {

    val codePackage: Option[String] = packageName(scope)

    try {

      val packageDir = codePackage.map(_.replaceAll("\\.", File.separator))
      val generatedPackageFile = packageDir.map(_ + File.separator + fileName).getOrElse(fileName)

      // create package structure
      packageDir.foreach( d=> Files.createDirectories(outputDir.resolve(d)))

      content(codePackage) map {
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


        val packageDecl = packageName.map(p => s"package $p").getOrElse("")
        s"""
         |$packageDecl
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

        val packageDecl = packageName.map(p => s"package $p\n\n").getOrElse("")
        ts.map {
          case t: ScalaClass => genType(t)
          case _ => ""
        }.mkString(
            packageDecl,
            "\n\n", ""
          ).success

    }
  }

  def genCodec(c: ScalaClass): String = {
    val propNames = c.properties.map(p => '"' + p.name + '"').mkString(", ")
    val className = c.identifier
    c.additionalNested match {
      case None =>
        s"""
       |  implicit def ${className}Codec: CodecJson[$className]=casecodec${c.properties.length}($className.apply, $className.unapply)($propNames)
     """.stripMargin
      case Some(additionalType) =>
        val addClassName = additionalType.identifier
        val addPropNames = propNames + (propNames.isEmpty ? "" | ", ") + '"' + addPropName + '"'
        s"""
       |    private def ${className}SimpleCodec: CodecJson[$className] = casecodec${c.properties.length + 1}($className.apply, $className.unapply)($addPropNames)
       |
       |    implicit def ${className}Codec: CodecJson[$className] = CodecJson.derived(EncodeJson {
       |      v =>
       |        val j = ${className}SimpleCodec.encode(v)
       |        val nj = j.field("$addPropName").fold(j)(a => j.deepmerge(a))
       |        nj.hcursor.downField("$addPropName").deleteGoParent.focus.getOrElse(nj)
       |    }, DecodeJson {
       |      c =>
       |        val md: DecodeJson[Option[Map[String, $addClassName]]] = implicitly
       |        val od: DecodeJson[$className] = ${className}SimpleCodec
       |        for {
       |          o <- od.decode(c)
       |          withoutProps = List($propNames).foldLeft(c)((c, f) => c.downField(f).deleteGoParent.hcursor.getOrElse(c))
       |          m <- md.decode(withoutProps)
       |        } yield o.copy($addPropName = m)
       |    })
       |
       """.stripMargin
    }
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
      val extra =
        t.additionalNested.map {
          tn =>
            val propType = genPropertyType(tn)
            // nested type is option , so that the special codec works even when no props are given
            s"$addPropName:Option[Map[String, $propType]]"
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


  def info(s: => String) = System.out.println(s)

  def debug(s: => String) = System.out.println(s)

  def error(s: => String) = System.err.println(s)

  private implicit class Printable[T](v: T) {
    def withInfo(prefix: String = ""): T = {
      info(s"$prefix : $v")
      v
    }

    def withError(prefix: String = ""): T = {
      error(s"$prefix : $v")
      v
    }

    def withDebug(prefix: String = ""): T = {
      debug(s"$prefix : $v")
      v
    }
  }

  def gen[N: Numeric, T: JsonSource](jsonParser: JsonSchemaParser[N], source: T)(codeGenTarget: Path) = {

    for {
      schema: SchemaDocument[N] <- jsonParser.parse(source).validation.withDebug("parsed schema")
      models: Set[ScalaType] <- ScalaModelGenerator(schema).withDebug("generated object model")
      modelFiles: Seq[Path] <- generateModel(models, schema.scope, codeGenTarget).withDebug("model files")
      codecFiles: Seq[Path] <- generateCodec(models, schema.scope, codeGenTarget).withDebug("serializatoin files")
    } yield (modelFiles ++ codecFiles).withInfo("generated files")

  }


}


