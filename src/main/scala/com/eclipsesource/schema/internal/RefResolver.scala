package com.eclipsesource.schema.internal

import java.net.URLDecoder

import com.eclipsesource.schema._
import play.api.data.mapping.Path
import play.api.libs.json.Json

import scala.io.{BufferedSource, Source}
import scala.util.Try

object RefResolver {

  // TODO: replace context parameter, unclear what for this is needed
  def replaceRefs(schema: QBType, context: Context): QBType = {
    schema match {

      case container: QBContainer =>
        container.updated(container.id, container.qbTypes.map(t => replaceRefs(t, context)):_*)

      case cls: QBClass =>
        // find and replace any ref, if available
        val substitutedType = cls.properties.collectFirst {
          case attr@QBAttribute(_, ref: QBRef, _) if !context.visited.contains(ref) => ref
        }.flatMap(ref => resolveRef(ref, context.copy(visited = context.visited + ref)) ).getOrElse(cls)
        substitutedType match {
          case c: QBClass => c.copy(rules = c.rules.map(rule => substituteRule(rule, context)))
          case x => x
        }

      case z => z
    }
  }

  def substituteRule(rule: ValidationRule, context: Context): ValidationRule = rule match {
    case schemaRule: SchemaBasedValidationRule => schemaRule.copy(
      schemas = schemaRule.schemas.map(schema => replaceRefs(schema, context))
    )
    case otherRule => otherRule
  }


  // TODO: switch to Either in order to provide more fine-grained error message
  def fetchSchema(url: String, context: Context): Option[QBType] = {
    val contents: BufferedSource = Source.fromURL(url)

    for {
      json <- Try { Json.parse(contents.getLines().mkString) }.toOption
      resolvedSchema <- Json.fromJson[QBType](json).asOpt
    } yield resolvedSchema
  }

  private[schema] def isRef: QBType => Boolean = {
    case ref@QBRef(_, _, false, _) => true
    case _ => false

  }

  private def isQBRefClass: QBType => Boolean = {
    case cls: QBClass if cls.properties.exists(p => isRef(p.qbType)) => true
    case _ => false
  }

  def resolveRef(path: String, context: Context): Option[QBType] = {
    if (path.isEmpty) {
      Some(context.root)
    } else {

      val resolved = resolveRef(context.root, toFragments(path), context)
      resolved.map(res => replaceRefs(res, context))
    }
  }

  def resolveRef(ref: QBRef, context: Context): Option[QBType] = {
    val r = context.id.getOrElse(ref.pointer.path)
    resolveRef(r, context)

//    if (ref.resolutionScope.isDefined) {
//      val r = ref.resolutionScope.get
//    resolveRef(r, context)
//    } else {
//      resolveRef(ref.pointer.path, context)
//    }
  }


  private def resolveRef(current: QBType, fragments: List[String], newContext: Context): Option[QBType] = {

    (current, fragments.headOption) match {

      case (_, Some(ref)) if ref.startsWith("http") =>
        for {
          schema <- fetchSchema(ref, newContext)
          result <- resolveRef(schema, fragments.tail, if (newContext.root == current) newContext.copy(root = schema)  else newContext )
        } yield result

      case (_, Some("#")) => resolveRef(newContext.root, fragments.tail, newContext)
      case (container: Resolvable, Some(fragment)) =>
        // TODO container is not supposed to contain a ref at this point
        // resolve single segment
        container.resolvePath(fragment).flatMap(resolvedType => {
          resolveRef(resolvedType, fragments.tail,
            newContext.copy(path = newContext.path.compose(Path(fragment))))
        })

      case (cls: QBClass, None) if isQBRefClass(cls)   =>
        val ref = cls.properties.collectFirst { case QBAttribute("$ref", ref: QBRef, _) => ref}
        if (ref.isDefined && ref.get.pointer.path == "#") {
          Some(cls)
        } else {
          for {
            r <- ref
            l <- resolveRef(r, newContext)
          } yield l
        }
      case (_, None) => Some(current)
      case _ => None
    }
  }

  // TODO rename
  private def toFragments(uri: String): List[String] = {
    val fragmentStartIndex = uri.indexOf("#")
    val fragments: List[String] = uri.find((c: Char) => c == '#').map(_ => uri.substring(fragmentStartIndex, uri.length)).getOrElse("").split("/").toList.map(segment =>
      // perform escaping
      URLDecoder.decode(segment, "UTF-8")
        .replace("~1", "/")
        .replace("~0", "~")
    )
    if (fragmentStartIndex < 0) {
      List(uri)
    } else if (fragmentStartIndex == 0) {
      fragments
    } else {
      val baseUri = uri.substring(0, fragmentStartIndex)
      List(baseUri) ++ fragments
    }
  }

}
