package com.eclipsesource.schema.internal

import java.net.{URI, URL, URLDecoder}

import com.eclipsesource.schema._

import play.api.data.mapping.Path
import play.api.libs.json.Json

import scala.io.{BufferedSource, Source}
import scala.util.Try

object RefResolver {

  private[schema] def normalize(path: String, context: Context): String = {

    def pathWithHash: String = if (!path.contains("#") && !path.endsWith("/")) s"$path#" else path
    def compose(scope: String) = {
      if (scope.endsWith("/"))
        scope + pathWithHash
      else scope + "/" + pathWithHash
    }
    def dropHashIfAny(scope: String) = if (scope.endsWith("#")) scope.dropRight(1) + path else scope
    val isAbsolute = Try { new URI(path) }.map(_.isAbsolute).getOrElse(false)

    path match {
      case p if p.startsWith("#") =>
        context.id.map(dropHashIfAny).getOrElse(path)
      case _ if isAbsolute =>
        pathWithHash
      case other =>
        val resolutionScope = if (context.isRootScope) findBaseUrl(context.id).map(_.toString) else context.id
        resolutionScope.map(compose).getOrElse(path)
    }
  }

  /**
    * Find a $ref in the given schema, if any and tries to resolve it.
    *
    * @param context the resolution context
    * @param schema the schema with a $ref, if any
    * @return the resolved schema
    */

  private[schema] def resolveRefIfAny(context: Context)(schema: SchemaType): Option[SchemaType] = {
    def findRef(schemaObject: SchemaObject): Option[RefAttribute] = schemaObject.properties.collectFirst {
      case ref: RefAttribute if !context.visited.contains(ref) => ref
    }
    schema match {
      case obj: SchemaObject if findRef(obj).isDefined =>
        for {
          unvisited <- findRef(obj)
          updatedContext = updateResolutionScope(context, obj).copy(visited = context.visited + unvisited)
          resolved  <- resolve(unvisited.pointer, updatedContext)
          r <- resolveRefIfAny(updatedContext)(resolved)
        } yield r

      case other => Some(other)
    }
  }

  def updateResolutionScope(context: Context, schema: SchemaType): Context = schema match {
    case withId: HasId =>
      val updatedScope = for {
        baseId <- context.id
        schemaId <- withId.id
      } yield normalize(schemaId, context)
      context.copy(id = updatedScope orElse context.id)
    case other => context
  }

  private[schema] def resolve(path: String, context: Context): Option[SchemaType] = {
    if (path.isEmpty) {
      Some(context.documentRoot)
    } else {
      GlobalContextCache.get(path).fold {
        for {
        // root is potentially a $ref
          root <- resolveRefIfAny(context)(context.documentRoot)
          resolved <- resolve(root, path, context.copy(documentRoot = root))
        } yield {
          GlobalContextCache.add(normalize(path, context))(resolved)
        }
      } { Some(_) }
    }
  }

  private def resolve(current: SchemaType, path: String, context: Context): Option[SchemaType] = {
    (current, path) match {

      case (_, pointer) if pointer.startsWith("http") =>
        val fragmentStartIndex = pointer.indexOf("#")
        val hasHash = fragmentStartIndex > 0
        val idx = if (hasHash) fragmentStartIndex + 1 else 0

        // change document root
        def resolveWithUpdatedRoot(resolved: SchemaType) = {
          if (hasHash) {
            val root = if (current == context.documentRoot) resolved else context.documentRoot
            resolve(resolved, "#" + pointer.substring(idx), context.copy(documentRoot = root))
          } else {
            Some(resolved)
          }
        }

        for {
          fetchedSchema <- fetchSchema(new URL(pointer))
          resolved <- resolveRefIfAny(context)(fetchedSchema)
          result <- resolveWithUpdatedRoot(resolved)
        } yield result

      case (schema,"#") => Some(context.documentRoot)

      case (schema, ref) if ref.startsWith("#") =>
        resolve(context.documentRoot, ref.substring(math.min(2, ref.length)), context)

      case (container: Resolvable, fragments) =>
        val resultOpt = for {
          resolved <- resolveFragments(toFragments(fragments), context, container)
        } yield resolved
        resultOpt orElse resolveRelative(fragments, context)

      case (_, _) => Some(current)
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
          res <- resolveFragments(rest,
            context.copy(
              schemaPath = context.schemaPath ++ Path(fragment),
              instancePath = context.instancePath ++ Path(fragment)
            ),
            resolved
          )
        // res is possibly a ref
          result <- resolveRefIfAny(context)(res)
        } yield result
    }
  }

  /**
    * Fetch a schema from the given URI.
    *
    * @param url the absolute URI string from which to fetch schema
    * @return a schema wrapped in an Option
    */
  private[schema] def fetchSchema(url: URL, recursiveResolve: Boolean = true): Option[SchemaType] = {
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
    for {
      normalized <- context.id.map(id => normalize(documentName, context))
      fetchedSchema   <- fetchSchema(new URL(normalized))
      res <- resolveRefIfAny(context)(fetchedSchema)
      resolved <- resolve(res,
        if (hasHash) relativeFragments.substring(hashIdx) else "#",
        context.copy(documentRoot = res)
      )
    } yield resolved
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
