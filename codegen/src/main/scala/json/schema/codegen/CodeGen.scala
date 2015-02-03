package json.schema.codegen

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import json.schema.parser.{JsonSchemaParser, SchemaDocument}
import json.source.JsonSource
import scala.util.control.NonFatal
import scalaz.Leibniz.===
import scalaz.Leibniz

import scalaz.std.AllInstances._
import scalaz.syntax.all._
import scalaz.syntax.std.all._

trait CodeGen extends Naming {
  type SValidation[T] = scalaz.Validation[String, T]


  val predefinedPackageCodec  = "json.schema.codegen.predefined"

  val addPropName = "_additional"

  def generateFile(codePackage: String, fileName: String, outputDir: Path)(content: Option[String] => SValidation[String]): SValidation[List[Path]] = {

    try {

      content(codePackage.some.noneIfEmpty) map {
        fileContent =>

          if (fileContent.trim.isEmpty)
            Nil
          else {

            val packageDir = codePackage.replaceAll("\\.", File.separator)

            // create package structure
            val fileDir: Path = outputDir.resolve(packageDir)

            if (!fileDir.toFile.exists())
              Files.createDirectories(fileDir)

            val generateAbsoluteFile: Path = fileDir.resolve(fileName)
            Files.deleteIfExists(generateAbsoluteFile)
            List(
              Files.write(generateAbsoluteFile, fileContent.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE_NEW)
            )

          }
      }

    } catch {
      case NonFatal(e) => e.toString.failure
    }

  }

  def generateCodec(outputDir: Path): SValidation[List[Path]] = {
    val codecClassName: String = "Codecs"
    val fileName: String = codecClassName.toLowerCase + ".scala"
    generateFile(predefinedPackageCodec, fileName, outputDir) {
      packageName =>

        val codecs= List(
          genCodecURI(),
          genCodecInetAddress("4"),
          genCodecInetAddress("6"),
          genCodecDate()
        ).filter(!_.trim.isEmpty).mkString("\n")

        if (codecs.isEmpty)
          "".success
        else {
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

  }



  def generateCodec(ts: Set[ScalaType], scope: String, outputDir: Path): SValidation[List[Path]] = {
    val codecClassName: String = "Codecs"
    val fileName: String = codecClassName + ".scala"

    val formatTypes: Set[ScalaType] =ScalaModelGenerator.format2scala.values.toSet

    def codecPackage(t: ScalaType) = formatTypes.contains(t) ?  (predefinedPackageCodec + "." +  codecClassName) | withPackageReference(t)(codecClassName)

    generateFile(scope, fileName, outputDir) {
      packageName =>

        val referencedTypes = ts.flatMap( _.referenced).filter(t => !t.scope.isEmpty && t.scope != scope)
        val referencedCodes = referencedTypes.isEmpty ? "" | referencedTypes.map( codecPackage ).toSet.mkString(" extends ", " with ", "")

        val codecs = ts.map {
          case t: ScalaClass => genCodecClass(t)
          case t: ScalaEnum => genCodecEnum(t)
          case _ => ""
        }.filter(!_.trim.isEmpty).mkString("\n")

        if (codecs.isEmpty)
          "".success
        else {
          val packageDecl = packageName.map(p => s"package $p").getOrElse("")
          s"""
          |$packageDecl
          |
          |import argonaut._, Argonaut._
          |
          |trait $codecClassName  $referencedCodes {
          |$codecs
          |}
          |
          |object $codecClassName extends $codecClassName
          """.stripMargin.success
        }

    }

  }

  def generateModel(ts: Set[ScalaType], scope: String, outputDir: Path): SValidation[List[Path]] = {
    val fileName: String = "model.scala"
    generateFile(scope, fileName, outputDir) {
      packageName =>

        val packageDecl = packageName.map(p => s"package $p\n\n").getOrElse("")
        val modelDecl = ts.map(genTypeDeclaration).filter(!_.trim.isEmpty)
        if (modelDecl.isEmpty)
          "".success
        else
          modelDecl.mkString(
            packageDecl,
            "\n\n", ""
          ).success

    }
  }

  def genCodecClass(c: ScalaClass): String = {
    val propNames = c.properties.map(p => '"' + p.name + '"').mkString(", ")
    val className = c.identifier

    c.additionalNested match {
      case None =>
        s"""
           |implicit def ${className}Codec: CodecJson[$className]=casecodec${c.properties.length}($className.apply, $className.unapply)($propNames)
     """.stripMargin
      case Some(additionalType) =>
        val addClassReference = genPropertyType(additionalType)
        val addPropNames = propNames + (propNames.isEmpty ? "" | ", ") + '"' + addPropName + '"'
        s"""
        |  private def ${className}SimpleCodec: CodecJson[$className] = casecodec${c.properties.length + 1}($className.apply, $className.unapply)($addPropNames)
        |
        |  implicit def ${className}Codec: CodecJson[$className] = CodecJson.derived(EncodeJson {
        |    v =>
        |      val j = ${className}SimpleCodec.encode(v)
        |      val nj = j.field("$addPropName").fold(j)(a => j.deepmerge(a))
        |      nj.hcursor.downField("$addPropName").deleteGoParent.focus.getOrElse(nj)
        |  }, DecodeJson {
        |    c =>
        |      val md: DecodeJson[Option[Map[String, $addClassReference]]] = implicitly
        |      val od: DecodeJson[$className] = ${className}SimpleCodec
        |      for {
        |        o <- od.decode(c)
        |        withoutProps = List($propNames).foldLeft(c)((c, f) => c.downField(f).deleteGoParent.hcursor.getOrElse(c))
        |        m <- md.decode(withoutProps)
        |      } yield o.copy($addPropName = m)
        |  })
       """.stripMargin
    }
  }

  def genCodecEnum(c: ScalaEnum): String = {
    val enumTypeName = c.identifier
    val enumNameReference = genPropertyType(c)
    val nestedTypeReference = genPropertyType(c.nested)
    s"""
    |implicit def ${enumTypeName}Codec: CodecJson[$enumNameReference] = CodecJson[$enumNameReference]((v: $enumNameReference) => v.${if (nestedTypeReference == "String") "toString" else "id"}.asJson, (j: HCursor) => j.as[$nestedTypeReference].flatMap {
    |  s: $nestedTypeReference =>
    |    try{
    |      DecodeResult.ok(${if (c.nested.identifier == "String") enumTypeName + ".withName" else enumTypeName}(s))
    |    } catch {
    |      case e:NoSuchElementException => DecodeResult.fail("$enumTypeName", j.history)
    |    }
    |})
     """.stripMargin
  }

  def genCodecURI(): String =
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

  def genCodecDate(): String =
    s"""
       |private def isoDate = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
       |implicit def DateCodec: CodecJson[java.util.Date] = CodecJson.derived(
       |  EncodeJson(v => jString(isoDate.format(v))),
       |  StringDecodeJson.flatMap {
       |    time =>
       |      DecodeJson(
       |        j => {
       |          try {
       |            DecodeResult.ok(isoDate.parse(time))
       |          } catch {
       |            case e: NoSuchElementException => DecodeResult.fail("Inet6Address", j.history)
       |          }
       |        }
       |      )
       |  })
    """.stripMargin

  def genCodecInetAddress(v: String): String =
    s"""
      |implicit def Inet${v}AddressCodec: CodecJson[java.net.Inet${v}Address] = CodecJson.derived(
      |  EncodeJson(v => jString(v.toString.substring(1))),
      |  StringDecodeJson.flatMap {
      |    addr =>
      |      DecodeJson(
      |        j => {
      |          try {
      |            DecodeResult.ok(java.net.InetAddress.getByName(addr).asInstanceOf[java.net.Inet${v}Address])
      |          } catch {
      |            case e: NoSuchElementException => DecodeResult.fail("Inet${v}Address", j.history)
      |          }
      |        }
      |      )
      |  })
    """.stripMargin


  private def withPackageReference(t: ScalaType)(name: => String): String = if (t.scope.isEmpty) name else t.scope + "." + name

  def genPropertyType(t: ScalaType): String = {
    t match {
      case a: ScalaArray =>
        val nestedType = genPropertyType(a.nested)
        if (a.unique) s"Set[$nestedType]" else s"List[$nestedType]"
      case a: ScalaEnum => withPackageReference(a)(a.identifier + ".Value")
      case a: ScalaType => withPackageReference(a)(a.identifier)
    }
  }

  def genPropertyType(p: ScalaTypeProperty): String = {
    val t = genPropertyType(p.isa)
    p.required ? t | s"Option[$t]"
  }

  def genTypeDeclaration(clazz: ScalaType): String = clazz match {
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

  private def packageModels(models: Set[ScalaType]): Map[String, Set[ScalaType]] = models.groupBy(_.scope)


  def gen[N: Numeric, T: JsonSource](jsonParser: JsonSchemaParser[N], sources: Seq[T])(codeGenTarget: Path) = {

    implicit val ev: ===[SValidation[List[Path]], SValidation[List[Path]]] = Leibniz.refl
    implicit val evdoc: ===[SValidation[SchemaDocument[N]], SValidation[SchemaDocument[N]]] = Leibniz.refl
    implicit val evset: ===[SValidation[Set[ScalaType]], SValidation[Set[ScalaType]]] = Leibniz.refl

    for {
      schemas: List[SchemaDocument[N]] <- sources.map( source => jsonParser.parse(source).validation.withDebug("parsed schema")).toList.sequence
      models: List[Set[ScalaType]] <- schemas.map ( schema => ScalaModelGenerator(schema).withDebug("generated object model")).toList.sequence
      modelsByPackage: Map[String, Set[ScalaType]] = packageModels(models.flatMap(_.toList).toSet)
      modelFiles <- modelsByPackage.map {
        case (packageName, packageModels) =>
          generateModel(packageModels, packageName, codeGenTarget).withDebug("model files")
      }.toList.sequence
      codecFiles <- modelsByPackage.map {
        case (packageName, packageModels) =>
          generateCodec(packageModels, packageName, codeGenTarget).withDebug("serializatoin files")
      }.toList.sequence
      predefinedCodecs: List[Path] <- generateCodec(codeGenTarget).withDebug("serialization files")
    } yield {
      val paths: List[Path] = predefinedCodecs ++ modelFiles.flatten ++ codecFiles.flatten
      paths.withInfo("generated files")
    }

  }


}


