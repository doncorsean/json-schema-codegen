package json.schema.codegen

import java.io.File
import java.nio.file.Path

import json.schema.parser.SchemaDocument

import scalaz.Leibniz
import scalaz.Leibniz.===
import scalaz.std.AllInstances._
import scalaz.syntax.all._

trait TypeScriptGenerator extends CodeGenerator with TypeScriptNaming {

  val fileName: String = "model.ts"

  def generateModel(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]] = {
    generateFile(scope, fileName, outputDir) {
      packageName =>

        val referencesBlock = ts.flatMap(referenceString).mkString("\n")

        val packageDecl = packageName.map(p => s"$referencesBlock\n\ndeclare module $p {\n\n").getOrElse("")
        val modelDecl = ts.map(genTypeDeclaration).filter(!_.trim.isEmpty)
        if (modelDecl.isEmpty)
          "".success
        else
          modelDecl.mkString(
            packageDecl,
            "\n\n", "\n\n}"
          ).success
    }
  }

  private def referenceString(lt: LangType): Set[String] = {
    val thisRef = withPackagePath(lt)(fileName)
    val thisRefPath = Option(new File(thisRef).getParentFile).map(_.toPath).getOrElse(new File("").toPath)

    lt.referenced.filter(_.scope.nonEmpty).map(t => withPackagePath(t)(fileName)).filter(_ != thisRef).map {
      ref =>
        val othersRef = new File(ref).toPath
        val relRef = thisRefPath.relativize(othersRef)
        s"""/// <reference path="$relRef" />"""
    }
  }

  private def withPackagePath(t: LangType)(name: => String): String = if (t.scope.isEmpty) name else t.scope.replace(".", "/") + "/" + name

  private def withPackageReference(t: LangType)(name: => String): String = if (t.scope.isEmpty) name else t.scope + "." + name

  def genPropertyType(t: LangType): String = {
    t match {
      case a: ArrayType =>
        val nestedType = genPropertyType(a.nested)
        if (a.unique) s"$nestedType[]/* a set */" else s"$nestedType[]"
      case a: LangType => withPackageReference(a)(a.identifier)
    }
  }

  def genPropertyType(p: LangTypeProperty): String = genPropertyType(p.isa)

  def genTypeDeclaration(clazz: LangType): String = clazz match {
    case t: ClassType =>
      val properties = t.properties.map {
        p =>
          val propType = genPropertyType(p)
          val member = memberName(p.name)
          if (p.required) s"$member:$propType;" else s"$member?:$propType;"
      }
      val extra =
        t.additionalNested.map {
          tn =>
            val propType = genPropertyType(tn)
            s"[key: string]: $propType;"
        }
      val members = "\n" + (properties ++ extra.toList).mkString("\n") + "\n"
      s"""interface ${t.identifier} {$members}""".stripMargin

    case t: EnumType =>
      val valueDeclarations = t.enums.map {
        case v: Int =>
          val valueId = s"v${v.toInt}"
          s"$valueId = ${v.toInt}"
        case v: Double =>
          val valueId = s"v${v.toInt}"
          s"$valueId = ${v.toInt}"
        case _ => "" // other enums are not support
      }.filter(_ != "").mkString(", ")
      s"""enum ${t.identifier} { $valueDeclarations }""".stripMargin

    case _ => ""

  }


  private def packageModels(models: Set[LangType]): Map[String, Set[LangType]] = models.groupBy(_.scope)


  def apply[N: Numeric](schemas: List[SchemaDocument[N]])(codeGenTarget: Path): SValidation[List[Path]] = {

    implicit val evdoc: ===[SValidation[SchemaDocument[N]], SValidation[SchemaDocument[N]]] = Leibniz.refl

    for {
      models: List[Set[LangType]] <- schemas.map(schema => TypeScriptModelGenerator(schema).withDebug("generated object model")).sequence
      modelsByPackage: Map[String, Set[LangType]] = packageModels(models.flatMap(_.toList).toSet)
      modelFiles <- modelsByPackage.map {
        case (packageName, packageModels) =>
          generateModel(packageModels, packageName, codeGenTarget).withDebug("model files")
      }.toList.sequence
    } yield {
      val paths: List[Path] = modelFiles.flatten
      info(s"generated ${paths.size} in ${codeGenTarget}")
      paths.withDebug("generated files")
    }

  }


}


object TypeScriptCmd extends GeneratorCommand(List(new TypeScriptGenerator with ConsoleLogging {}))
