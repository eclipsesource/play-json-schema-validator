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
        val id = if (schema != context.root) {
          for {
            baseId <- context.id
            schemaId <- container.id
          } yield baseId + schemaId
        } else {
          context.id
        }.fold(context.id)(i => Some(i))
        container.updated(container.id, container.qbTypes.map(t => replaceRefs(t, context.copy(id = id))):_*)
      case cls: QBClass =>

        val id = (if (schema != context.root) {
          for {
            baseId <- context.id
            schemaId <- cls.id
          } yield baseId + schemaId
        } else {
          context.id
        }).fold(context.id)(i => Some(i))

        // find and replace any ref, if available
        val substitutedType = cls.properties.collectFirst {
          case attr@QBAttribute(_, ref: QBRef, _) if !context.visited.contains(ref) => ref
        }.flatMap(ref => resolveRef(ref, context.copy(visited = context.visited + ref, id = id)) ).getOrElse(cls)
//        println("unsubstituted type is " + cls)
//        println("substituted type is "+ substitutedType)
        substitutedType match {
          // also replace any rules that contain schemas
          // TODO: provide common
          case c: QBClass => c.copy(rules = c.rules.map(rule => substituteRule(rule, context.copy(id = id))))
//            c.copy(rules = c.rules.map {
//            case QBAllOfRule(schemas) => QBAllOfRule(schemas.map(s => replaceRefs(s, context)))
//            case QBAnyOfRule(schemas) => QBAnyOfRule(schemas.map(s => replaceRefs(s, context)))
//            case QBOneOfRule(schemas) => QBOneOfRule(schemas.map(s => replaceRefs(s, context)))
//            case z => z
//          })
          case x => x
        }
      case z => /*println("hi " + z);*/ z
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

  private def isRemoteRef(attr: QBAttribute): Boolean = attr.name == "$ref" && isRemoteRef(attr.qbType)

  private def isRemoteRef: QBType => Boolean = {
    case r@QBRef(_, _, true) => true
    case cls: QBClass if cls.properties.exists(isRemoteRef(_)) => true
    case _ => false
  }

  private[schema] def isRef: QBType => Boolean = {
//    case cls: QBClass if cls.properties.exists(p => isRef(p.qbType)) => true
    case ref@QBRef(_, _, false) => true
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
    val r = context.id.getOrElse("") + ref.pointer.path
    resolveRef(r, context)
  }


  private def resolveRef(current: QBType, fragments: List[String], newContext: Context): Option[QBType] = {

//    println("-- " + fragments.size)
//    println("current is " + current)
//    println("fragment head is " + fragments.headOption)

    (current, fragments.headOption) match {

      case (_, Some(ref)) if ref.startsWith("http") =>
        for {
          schema <- fetchSchema(ref, newContext)
          result <- resolveRef(schema, fragments.tail, if (newContext.root == current) newContext.copy(root = schema)  else newContext )
        } yield result

      // if object contains a $ref, resolve it and try again
//      case (obj: QBClass, _) if isQBRefClass(obj) =>
//        for {
//          ref <- obj.properties.collectFirst { case QBAttribute("$ref", ref: QBRef, _) => ref}
//          result <- resolveRef(ref, fragments, newContext)
//          lol <- resolveRef(result, fragments, if (newContext.root == current) newContext.copy(root = result)  else newContext )
//        } yield result

//      case (_, Some("#")) if fragments.tail.isEmpty => Some(newContext.root)
      case (_, Some("#")) => resolveRef(newContext.root, fragments.tail, newContext)
      case (container: Resolvable, Some(fragment)) =>
        // TODO container is not supposed to contain a ref at this point
        // resolve single segment
        container.resolvePath(fragment).flatMap(resolvedType => {

          // TODO: ugly, is this actually needed?
          val f: List[String] = if (isRef(resolvedType)) {
            toFragments(resolvedType.asInstanceOf[QBClass].properties.collectFirst {
              case QBAttribute(_, ref: QBRef, _) => ref.pointer.path
            }.getOrElse("")) ++ fragments.tail
          } else {
            fragments.tail
          }
          resolveRef(resolvedType, f.filterNot(_ == ""), //fragments.tail,
            newContext.copy(path = newContext.path.compose(Path(fragment))))
        })

      case (ref: QBRef, None) => resolveRef(ref.pointer.path, newContext.copy(visited =  newContext.visited + ref))
      case (cls: QBClass, None) if isQBRefClass(cls)   =>
//        println("TU ES@!!")
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
//    println("stripBaseUri " + uri)
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
