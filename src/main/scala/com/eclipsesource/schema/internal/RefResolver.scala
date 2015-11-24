package com.eclipsesource.schema.internal

import java.net.{URI, URL, URLDecoder}

import com.eclipsesource.schema._

import play.api.data.mapping.Path
import play.api.libs.json.Json

import scala.io.{BufferedSource, Source}
import scala.util.Try

object RefResolver {

  private def normalize(path: String, context: Context): String = {

    //def dropEndingHash(scope: String) = if (scope.endsWith("#")) scope.dropRight(1) else scope
    def compose(scope: String) =  if (scope.endsWith("/")) scope + path else scope + "/" + path

    val resolutionScope = if (context.isRootScope) findBaseUrl(context.id).map(_.toString) else context.id
    val isAbsolute = Try { new URI(path) }.map(_.isAbsolute).getOrElse(false)

    if (isAbsolute) {
      path
    } else {
      resolutionScope.map(compose).getOrElse(path)
    }
  }

  /**
    * Resolve all refs in the given schema.
    *
    * @param context the resolution context
    * @param schema the schema in which all refs should be resolved
    * @return the schema with all refs resolved
    */
  def resolveAll(context: Context)(schema: SchemaType): SchemaType = {
    def findRef(schemaObject: SchemaObject): Option[RefAttribute] = schemaObject.properties.collectFirst {
      case ref@RefAttribute(_, _) if !context.visited.contains(ref.pointer) => ref
    }

    schema match {
      case container: SchemaArrayLike =>
        container.updated(resolveAll(updateResolutionScope(context, container)))

      case obj: SchemaObject =>
        val (substitutedType, updContext) = (for {
          unvisited <- findRef(obj)
          updatedContext = updateResolutionScope(context, obj).copy(visited = context.visited + unvisited.pointer)
          resolved  <- resolve(unvisited.pointer, updatedContext)
        } yield (resolved, updatedContext)).getOrElse((obj, context))

        substitutedType.updated(resolveAll(updContext))

      case other => other
    }
  }

  private def updateResolutionScope(context: Context, schema: SchemaType): Context = schema match {
    case withId: HasId =>
      val updatedScope = for {
        baseId <- context.id
        schemaId <- withId.id
      } yield normalize(schemaId, context)
      context.copy(id = updatedScope orElse context.id)
    case other => context
  }

  def resolve(path: String, context: Context): Option[SchemaType] = {
    if (path.isEmpty) {
      Some(context.documentRoot)
    } else {
      GlobalContextCache.get(path).fold {
        val resolved = resolve(context.documentRoot, path, context)
        resolved.map(GlobalContextCache.add(path))
      } { Some(_) }
    }
  }

  private[schema] def resolve(current: SchemaType, path: String, context: Context): Option[SchemaType] = {
    (current, path) match {

      case (_, ref) if ref.startsWith("http") =>
        val fragmentStartIndex = ref.indexOf("#")
        val hasHash = fragmentStartIndex > 0
        val idx = if (hasHash) fragmentStartIndex + 1 else 0

        fetchSchema(new URL(ref)).flatMap(resolved =>
          if (hasHash) {
            val root = if (current == context.documentRoot) resolved else context.documentRoot
            resolve(resolved, "#" + ref.substring(idx), context.copy(documentRoot = root))
          } else {
            Some(resolved)
          }
        )

      case (schema,"#") => Some(context.documentRoot)

      case (schema, ref) if ref.startsWith("#") =>
        resolve(context.documentRoot, ref.substring(math.min(2, ref.length)), context)

      case (container: Resolvable, fragments) =>
        //        println(s"Fragments or relative ${container.prettyPrint} / $fragments")
        val resultOpt = for {
          resolved <- resolveFragments(toFragments(fragments), context, container)
          result   <- resolveRefIfAny(resolved, updateResolutionScope(context, resolved))
        } yield result
        resultOpt orElse resolveRelative(fragments, context)

      case (_, _) => Some(current)
    }
  }

  /**
    * Resolve ref if the given schema contains any.
    *
    * @param schema the schema
    * @param context the resolution context
    * @return the resolved ref, if any, otherwise None
    */
  private def resolveRefIfAny(schema: SchemaType, context: Context): Option[SchemaType] = {
    def hasRef(obj: SchemaObject): Boolean =  obj.properties.exists(_.name == "$ref")

    schema match {
      case obj: SchemaObject if hasRef(obj) =>
        for {
          ref      <- obj.properties.collectFirst { case ref: RefAttribute => ref }
          resolved <- resolve(ref.pointer, context.copy(visited =  context.visited + ref.pointer))
        } yield resolved
      case other => Some(other)
    }
  }

  /**
    * Resolve a given list of fragments against a given schema.
    *
    * @param fragments a list of single fragments to be resolved
    * @param context the initial context
    * @param schema the scheam to resolve against the single fragments
    * @return the resolved schema
    */
  private def resolveFragments(fragments: List[String], context: Context, schema: SchemaType): Option[SchemaType] = {
    (fragments, schema) match {
      case (Nil, result) => Some(result)
      case (fragment :: rest, resolvable: Resolvable) =>
        for {
          resolved <- resolvable.resolvePath(fragment)
          result <- resolveFragments(rest,
            context.copy(
              schemaPath = context.schemaPath.compose(Path(fragment)),
              instancePath = context.instancePath.compose(Path(fragment))
            ),
            resolved
          )
        } yield result
    }
  }

  /**
    * Fetch a schema from the given URI.
    *
    * @param url the absolute URI string from which to fetch schema
    * @return a schema wrapped in an Option
    */
  private[schema] def fetchSchema(url: URL): Option[SchemaType] = {
    // TODO: switch to Either in order to provide more fine-grained error message
    GlobalContextCache.get(url.toString) match {
      case cached@Some(_) => cached
      case otherwise =>
        val contents: BufferedSource = Source.fromURL(url)
        val resolved = for {
          json <- Try { Json.parse(contents.getLines().mkString) }.toOption
          resolvedSchema <- Json.fromJson[SchemaType](json).asOpt
        } yield resolvedSchema
        resolved.map { GlobalContextCache.add(url.toString) }
    }
  }

  /**
    * Finds out the actual base URI based on a given resolution scope.
    *
    * @param scope the resolution scope from which to determine the base URL
    * @return the base URL, if any, otherwise None
    */
  private def findBaseUrl(scope: Option[String]): Option[URL] = {
    scope.map(url => {
      val schemaUrl = new URL(url)
      val protocol = schemaUrl.getProtocol
      val host = schemaUrl.getHost
      val port = schemaUrl.getPort
      val file = schemaUrl.getFile

      if (schemaUrl.getHost.nonEmpty) {
        if (schemaUrl.getPort != -1) {
          new URL(s"$protocol://$host:$port")
        } else {
          new URL(s"$protocol://$host")
        }
      } else {
        new URL(s"$protocol://${file.substring(0, file.lastIndexOf("/"))}")
      }
    })
  }

  /**
    * Resolve the given fragments relatively against the base URL.
    *
    * @param relativeFragments a fragments to be resolved
    * @param context the resolution context
    * @return the resolved schema
    */
  private def resolveRelative(relativeFragments: String, context: Context): Option[SchemaType] = {
    val hashIdx = relativeFragments.indexOf("#")
    val hasHash = hashIdx != -1
    val documentName = if (hasHash) relativeFragments.substring(0, hashIdx) else relativeFragments
    val normalized = context.id.map(id => normalize(documentName, context))
    val r= for {
      n <- normalized
      schema   <- fetchSchema(new URL(n))
      resolved <- resolve(schema,
        if (hasHash) relativeFragments.substring(hashIdx) else "#",
        context.copy(documentRoot = schema)
      )
    } yield resolved
    r
  }

  /**
    * Split the given URI string into single fragments.
    * If the given URI is an absolute path the base URI
    * will be ignored.
    *
    * @param uri the URI to be split
    * @return the single fragments as a list
    */
  private def toFragments(uri: String): List[String] = {

    def escape(s: String): String =
      URLDecoder.decode(s, "UTF-8")
        .replace("~1", "/")
        .replace("~0", "~")

    def withoutBaseUri(s: String): Option[String] = {
      s.find(_ == '#')
        .map(_ => uri.substring(uri.indexOf("#")))
    }

    val withoutBase = withoutBaseUri(uri)
    withoutBase.getOrElse(uri)
      .split("/").toList
      .map(escape)
  }
}
