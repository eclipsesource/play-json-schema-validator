package com.eclipsesource.schema.internal

import java.net.{URL, URLDecoder}
import java.util.Date

import com.eclipsesource.schema._
import play.api.data.mapping.Path
import play.api.libs.json.Json

import scala.io.{BufferedSource, Source}
import scala.util.Try

object RefResolver {

  def replaceRefs(context: Context)(schema: SchemaType): SchemaType = {

    def findFirstUnvisitedRef(schemaObject: SchemaObject): Option[SchemaRef] = schemaObject.properties.collectFirst {
      case attr@SchemaAttribute(_, ref: SchemaRef) if !context.visited.contains(ref) => ref
    }

    schema match {
      case container: SchemaArrayLike =>
        val id = updateContextId(context, container)
        container.updated(replaceRefs(context.copy(id = id)))

      case cls: SchemaObject =>

        val id = updateContextId(context, cls)
        val (substitutedType, updContext) = (for {
          unvisited <- findFirstUnvisitedRef(cls)
          c = context.copy(visited = context.visited + unvisited, id = id)
          resolved <- resolveRelativeRef(unvisited, c)
        } yield (resolved, c)).getOrElse((cls, context))

        substitutedType.updated(replaceRefs(updContext))

      case other => other
    }
  }

  def updateContextId(context: Context, container: HasId): Option[String] = {
    (if (container != context.root) {
      for {
        baseId <- context.id
        schemaId <- container.id
      } yield baseId + schemaId
    } else {
      context.id
    }).fold(context.id)(i => Some(i))
  }

  private def isRef: SchemaType => Boolean = {
    case ref@SchemaRef(_, _, false) => true
    case _ => false

  }

  private def isSchemaRefClass: SchemaType => Boolean = {
    case cls: SchemaObject if cls.properties.exists(p => isRef(p.schemaType)) => true
    case _ => false
  }

  /**
    * Entry point
    * @param path
    * @param context
    * @return
    */
  def resolveRef(path: String, context: Context): Option[SchemaType] = {
    if (path.isEmpty) {
      Some(context.root)
    } else {
      println(">> RESOLVINg " + path)
      GlobalContextCache.get(path).fold {
        val resolved = resolveRef(context.root, toFragments(path), context)
        resolved
          .map(res => replaceRefs(context)(res))
          .map(res => GlobalContextCache.add(path, res))
      } { result => Some(result) }
    }
  }

  def resolveRelativeRef(ref: SchemaRef, context: Context): Option[SchemaType] = {
    val r = context.id.getOrElse("") + ref.pointer.path
    resolveRef(r, context)
  }


  private[schema] def resolveRef(current: SchemaType, fragments: List[String], context: Context): Option[SchemaType] = {
    println("Resolve " + new Date() + " to " + fragments)
    (current, fragments.headOption) match {

      case (_, Some(ref)) if ref.startsWith("http") =>

        resolveRemoteRef(current, fragments, context, ref)

      case (_, Some("#")) =>
        resolveRef(context.root, fragments.tail, context)

      case (cls: SchemaObject, None) if isSchemaRefClass(cls)   =>
        for {
          ref <- cls.properties.collectFirst { case SchemaAttribute("$ref", schemaRef: SchemaRef) => schemaRef }
          resolved <- resolveRelativeRef(ref, context)
        } yield resolved

      case (container: Resolvable, Some(fragment)) =>
        // container is not supposed to contain a ref at this point
        // resolve single property or URL fragment
        resolveFragment(fragments, context, container, fragment) orElse
          resolveUrl(context.baseUrl, fragment)


      case (ref: SchemaRef, None) =>
        resolveRef(ref.pointer.path, context.copy(visited =  context.visited + ref))

      // resolve constraints
      case (s, Some(c)) =>
        for {
          resolved <- s.constraints.any.resolvePath(c)
          r <- resolveRef(resolved, fragments.tail, context)
        } yield r


      case (_, None) => Some(current)
    }
  }

  private def resolveFragment(fragments: List[String], context: Context, container: SchemaType with Resolvable, fragment: String): Option[SchemaType] = {
    for {
      resolved <- container.resolvePath(fragment)
      result <- resolveRef(resolved, fragments.tail,
        context.copy(
          schemaPath = context.schemaPath.compose(Path(fragment)),
          instancePath = context.instancePath.compose(Path(fragment))

        ))
    } yield result
  }


  // TODO: switch to Either in order to provide more fine-grained error message
  private[schema] def fetchSchema(url: String): Option[SchemaType] = {

    GlobalContextCache.get(url) match {
      case cached@Some(_) => cached
      case otherwise =>
        val contents: BufferedSource = Source.fromURL(url)
        val resolved = for {
          json <- Try { Json.parse(contents.getLines().mkString) }.toOption
          resolvedSchema <- Json.fromJson[SchemaType](json).asOpt
        } yield resolvedSchema
        resolved.foreach { r => GlobalContextCache.add(url, r) }
        resolved
    }
  }

  private def resolveUrl(baseUrl: Option[URL], fragment: String): Option[SchemaType] = {
    val url: Option[String] = for {
      bp <- baseUrl
    } yield new URL(s"${bp.toString}/$fragment").toString
    url.flatMap(u => fetchSchema(u))
  }

  private def resolveRemoteRef(current: SchemaType, fragments: List[String], context: Context, ref: String): Option[SchemaType] = {
    for {
      schema <- fetchSchema(ref)
      result <- resolveRef(schema, fragments.tail, if (context.root == current) context.copy(root = schema) else context)
    } yield result
  }

  private def toFragments(uri: String): List[String] = {
    val fragmentStartIndex = uri.indexOf("#")
    val fragments: List[String] = uri.find((c: Char) => c == '#')
      .map(_ => uri.substring(fragmentStartIndex, uri.length))
      .getOrElse("").split("/").toList
      .map(
        segment =>
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
