package com.eclipsesource.schema.internal

import java.net.URLDecoder

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.constraints.Constraints.Constraint
import play.api.data.mapping.Path
import play.api.libs.json.Json

import scala.io.{BufferedSource, Source}
import scala.util.Try

object RefResolver {

  // TODO: replace context parameter, unclear what for this is needed
  def replaceRefs(context: Context)(schema: SchemaType): SchemaType = {
    schema match {
      case container: SchemaContainer =>
        val id = if (schema != context.root) {
          for {
            baseId <- context.id
            schemaId <- container.id
          } yield baseId + schemaId
        } else {
          context.id
        }.fold(context.id)(i => Some(i))
        container.updated(container.id, container.schemaTypes.map(t => replaceRefs(context.copy(id = id))(t)):_*)
      case cls: SchemaObject =>

        val id = (if (schema != context.root) {
          for {
            baseId <- context.id
            schemaId <- cls.id
          } yield baseId + schemaId
        } else {
          context.id
        }).fold(context.id)(i => Some(i))

        // find and replace any ref, if available
        val (substitutedType, updContext) = cls.properties.collectFirst {
          case attr@SchemaAttribute(_, ref: SchemaRef, _) if !context.visited.contains(ref) => ref
        }.flatMap(ref => {
          val c = context.copy(visited = context.visited + ref, id = id)
          resolveRef(ref, c).map(res => (res, c))
        }).getOrElse((cls, context))
        substitutedType match {
          // if resolved type is a class, resolve all refs it contains
          case c: SchemaObject => c.copy(constraints = c.constraints.updated(replaceRefs(updContext)))
          case x => x
        }
      case z => z
    }
  }

  // TODO: switch to Either in order to provide more fine-grained error message
  def fetchSchema(url: String, context: Context): Option[SchemaType] = {
    val contents: BufferedSource = Source.fromURL(url)

    for {
      json <- Try { Json.parse(contents.getLines().mkString) }.toOption
      resolvedSchema <- Json.fromJson[SchemaType](json).asOpt
    } yield resolvedSchema
  }

  private def isRemoteRef(attr: SchemaAttribute): Boolean = attr.name == "$ref" && isRemoteRef(attr.schemaType)

  private def isRemoteRef: SchemaType => Boolean = {
    case r@SchemaRef(_, _, true) => true
    case cls: SchemaObject if cls.properties.exists(isRemoteRef(_)) => true
    case _ => false
  }

  private[schema] def isRef: SchemaType => Boolean = {
    case ref@SchemaRef(_, _, false) => true
    case _ => false

  }

  // TODO: is this ever called?
  private def isSchemaRefClass: SchemaType => Boolean = {
    case cls: SchemaObject if cls.properties.exists(p => isRef(p.schemaType)) => true
    case _ => false
  }

  def resolveRef(path: String, context: Context): Option[SchemaType] = {
    if (path.isEmpty) {
      Some(context.root)
    } else {
      val resolved = resolveRef(context.root, toFragments(path), context)
      resolved.map(res => replaceRefs(context)(res))
    }
  }

  def resolveRef(ref: SchemaRef, context: Context): Option[SchemaType] = {
    val r = context.id.getOrElse("") + ref.pointer.path
    resolveRef(r, context)
  }


  private def resolveRef(current: SchemaType, fragments: List[String], newContext: Context): Option[SchemaType] = {

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

          // TODO: ugly, is this actually needed?
          val f: List[String] = if (isRef(resolvedType)) {
            toFragments(resolvedType.asInstanceOf[SchemaObject].properties.collectFirst {
              case SchemaAttribute(_, ref: SchemaRef, _) => ref.pointer.path
            }.getOrElse("")) ++ fragments.tail
          } else {
            fragments.tail
          }
          resolveRef(resolvedType, f.filterNot(_ == ""), //fragments.tail,
            newContext.copy(path = newContext.path.compose(Path(fragment))))
        })

      case (ref: SchemaRef, None) => resolveRef(ref.pointer.path, newContext.copy(visited =  newContext.visited + ref))
      case (cls: SchemaObject, None) if isSchemaRefClass(cls)   =>
        val ref = cls.properties.collectFirst { case SchemaAttribute("$ref", ref: SchemaRef, _) => ref}
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
