package json.schema.codegen

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import json.schema.parser.{JsonSchemaParser, SchemaDocument}
import json.source.JsonSource
import scala.util.control.NonFatal
import scalaz.Validation


trait CodeGen extends Naming {

  import scalaz.Scalaz._

  val addPropName = "_additional"

  def generateFile(scope: URI, fileName: String, outputDir: Path)(content: Option[String] => Validation[String, String]): Validation[String, Seq[Path]] = {

    val codePackage: String = packageName(scope)

    try {

      val packageDir = codePackage.replaceAll("\\.", File.separator)

      // create package structure
      val fileDir: Path = outputDir.resolve(packageDir)

      if (!fileDir.toFile.exists())
        Files.createDirectories(fileDir)

      content(codePackage.some.noneIfEmpty) map {
        fileContent =>
          val generateAbsoluteFile: Path = fileDir.resolve(fileName)
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
    val codecClassName: String = className(scope) + "Codec"
    val fileName: String = codecClassName + ".scala"
    generateFile(scope, fileName, outputDir) {
      packageName =>

        val codecs = ts.map {
          case t: ScalaClass => genCodec(t)
          case t: ScalaEnum => genCodec(t)
          case ScalaSimple("java.net.URI") => genCodecURI()
          case ScalaSimple("java.net.Inet4Address") => genCodecInetAddress("4")
          case ScalaSimple("java.net.Inet6Address") => genCodecInetAddress("6")
          case ScalaSimple("java.util.Date") => genCodecDate()
          case _ => ""
        }.mkString("\n")


        val packageDecl = packageName.map(p => s"package $p").getOrElse("")
        s"""
         |$packageDecl
         |
         |import argonaut._, Argonaut._
         |
         |trait $codecClassName {
         |$codecs
         |}
         |
         |object $codecClassName extends $codecClassName
        """.stripMargin.success

    }

  }

  def generateModel(ts: Iterable[ScalaType], scope: URI, outputDir: Path): Validation[String, Seq[Path]] = {
    val fileName: String = className(scope) + ".scala"
    generateFile(scope, fileName, outputDir) {
      packageName =>

        val packageDecl = packageName.map(p => s"package $p\n\n").getOrElse("")
        ts.map(genType).mkString(
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

  def genCodec(c: ScalaEnum): String = {
    val className = c.identifier
    s"""
       |  implicit def ${className}Codec: CodecJson[$className.Value] = CodecJson[$className.Value]((v: $className.Value) => v.${if (c.nested.identifier == "String") "toString" else "id"}.asJson, (j: HCursor) => j.as[${c.nested.identifier}].flatMap {
       |    s: ${c.nested.identifier} =>
       |      try{
       |        DecodeResult.ok(${if (c.nested.identifier == "String") className + ".withName" else className}(s))
       |      } catch {
       |        case e:NoSuchElementException => DecodeResult.fail("$className", j.history)
       |      }
       |  })
     """.stripMargin
  }

  def genCodecURI() =
    """
      |  implicit def URICodec: CodecJson[java.net.URI] = CodecJson.derived(
      |    EncodeJson(v => jString(v.toString)),
      |    StringDecodeJson.flatMap {
      |      uri =>
      |        DecodeJson(
      |          j => {
      |            try{
      |              DecodeResult.ok(new java.net.URI(uri))
      |            } catch {
      |              case e:NoSuchElementException => DecodeResult.fail("URI", j.history)
      |            }
      |          }
      |        )
      |    })
    """.stripMargin

  def genCodecDate() =
    s"""
      |  private def isoDate = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
      |  implicit def DateCodec: CodecJson[java.util.Date] = CodecJson.derived(
      |    EncodeJson(v => jString(isoDate.format(v))),
      |    StringDecodeJson.flatMap {
      |      time =>
      |        DecodeJson(
      |          j => {
      |            try {
      |              DecodeResult.ok(isoDate.parse(time))
      |            } catch {
      |              case e: NoSuchElementException => DecodeResult.fail("Inet6Address", j.history)
      |            }
      |          }
      |        )
      |    })
    """.stripMargin

  def genCodecInetAddress(v: String) =
    s"""
      |  implicit def Inet${v}AddressCodec: CodecJson[java.net.Inet${v}Address] = CodecJson.derived(
      |    EncodeJson(v => jString(v.toString.substring(1))),
      |    StringDecodeJson.flatMap {
      |      addr =>
      |        DecodeJson(
      |          j => {
      |            try {
      |              DecodeResult.ok(java.net.InetAddress.getByName(addr).asInstanceOf[java.net.Inet${v}Address])
      |            } catch {
      |              case e: NoSuchElementException => DecodeResult.fail("Inet${v}Address", j.history)
      |            }
      |          }
      |        )
      |    })
    """.stripMargin

  def genPropertyType(t: ScalaType): String = {
    t match {
      case a: ScalaArray =>
        val nestedType = genPropertyType(a.nested)
        if (a.unique) s"Set[$nestedType]" else s"List[$nestedType]"
      case a: ScalaEnum => a.identifier + ".Value"
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
          val member = memberName(p.name)
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
          val valueId = memberName(v)
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


