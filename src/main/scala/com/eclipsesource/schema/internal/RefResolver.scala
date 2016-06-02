package com.eclipsesource.schema.internal

import java.net.{URI, URL, URLDecoder}

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import play.api.libs.json.{JsPath, Json}

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

class RefResolver extends UrlStreamResolverFactory {

  private final val WithProtocol = "^([^:\\/?]+):.+"
  private final val ProtocolPattern = WithProtocol.r.pattern

  private[schema] def normalize(path: String, scope: ResolutionScope): String = {

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
        scope.id.map(dropHashIfAny).getOrElse(path)
      case _ if isAbsolute =>
        pathWithHash
      case other =>
        val resolutionScope = if (scope.isRootScope) findBaseUrl(scope.id).map(_.toString) else scope.id
        resolutionScope.map(compose).getOrElse(path)
    }
  }

  /**
    * Find a $ref in the given schema, if any and tries to resolve it.
    *
    * @param scope the resolution context
    * @param schema the schema with a $ref, if any
    * @return the resolved schema
    */

  private[schema] def resolveRefIfAny(scope: ResolutionScope)(schema: SchemaType): Option[SchemaType] = {
    def findRef(schemaObject: SchemaObject): Option[RefAttribute] = schemaObject.properties.collectFirst {
      case ref: RefAttribute if !scope.hasBeenVisited(ref) => ref
    }
    schema match {
      case obj: SchemaObject if findRef(obj).isDefined =>
        for {
          unvisited <- findRef(obj)
          updatedScope = updateResolutionScope(scope, obj).addVisited(unvisited)
          resolved  <- resolve(unvisited.pointer, updatedScope)
          r <- resolveRefIfAny(updatedScope)(resolved)
        } yield r

      case other => Some(other)
    }
  }

  def updateResolutionScope(scope: ResolutionScope, schema: SchemaType): ResolutionScope = schema match {
    case withId: HasId =>
      val updatedScope = for {
        baseId <- scope.id
        schemaId <- withId.id
      } yield normalize(schemaId, scope)
      scope.copy(id = updatedScope orElse scope.id)
    case other => scope
  }

  private[schema] def resolve(path: String, scope: ResolutionScope): Option[SchemaType] = {
    if (path.isEmpty) {
      Some(scope.documentRoot)
    } else {
      GlobalContextCache.get(path).fold {
        for {
          // root is potentially a $ref
          root <- resolveRefIfAny(scope)(scope.documentRoot)
          resolved <- resolve(root, path, scope.copy(documentRoot = root))
        } yield {
          GlobalContextCache.add(normalize(path, scope))(resolved)
        }
      } { Some(_) }
    }
  }

  private def hasProtocol(path: String): Boolean = path.matches(WithProtocol)

  private def createUrl(pointer: String) = {
    // find out whether pointer contains URL and map to URLStreamHandler
    val matcher = ProtocolPattern.matcher(pointer)
    matcher.find()
    val protocol = Try { matcher.group(1).replaceAll("[^A-Za-z]+", "") }
    protocol match {
      case Success(p) =>
        new URL(null, pointer, createURLStreamHandler(p))
      case Failure(_) => new URL(pointer)
    }
  }

  private def resolve(current: SchemaType, path: String, scope: ResolutionScope): Option[SchemaType] = {

    (current, path) match {

      case (_, pointer) if hasProtocol(pointer) =>
        val fragmentStartIndex = pointer.indexOf("#")
        val hasHash = fragmentStartIndex > 0
        val idx = if (hasHash) fragmentStartIndex + 1 else 0

        // change document root
        def resolveWithUpdatedRoot(resolved: SchemaType) = {
          if (hasHash) {
            val root = if (current == scope.documentRoot) resolved else scope.documentRoot
            resolve(resolved, "#" + pointer.substring(idx), scope.copy(documentRoot = root))
          } else {
            Some(resolved)
          }
        }

        for {
          fetchedSchema <- fetchSchema(createUrl(pointer))
          resolved <- resolveRefIfAny(scope)(fetchedSchema)
          result <- resolveWithUpdatedRoot(resolved)
        } yield result

      case (schema,"#") => Some(scope.documentRoot)

      case (schema, ref) if ref.startsWith("#") =>
        resolve(scope.documentRoot, ref.substring(math.min(2, ref.length)), scope)

      case (container: Resolvable, fragments) =>
        val resultOpt = for {
          resolved <- resolveFragments(toFragments(fragments), scope, container)
        } yield resolved
        resultOpt orElse resolveRelative(fragments, scope)

      case (_, _) => Some(current)
    }
  }

  /**
    * Resolve a given list of fragments against a given schema.
    *
    * @param fragments a list of single fragments to be resolved
    * @param scope the initial resolution scope
    * @param schema the scheam to resolve against the single fragments
    * @return the resolved schema
    */
  private def resolveFragments(fragments: List[String], scope: ResolutionScope, schema: SchemaType): Option[SchemaType] = {
    (fragments, schema) match {
      case (Nil, result) => Some(result)
      case (fragment :: rest, resolvable: Resolvable) =>
        for {
          resolved <- resolvable.resolvePath(fragment)
          res <- resolveFragments(rest,
            scope.copy(
              schemaPath = scope.schemaPath.compose(JsPath \ fragment),
              instancePath = scope.instancePath.compose(JsPath \ fragment)
            ),
            resolved
          )
          // res is possibly a ref
          result <- resolveRefIfAny(scope)(res)
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
      val schemaUrl = createUrl(url)
      val protocol = schemaUrl.getProtocol
      val host = schemaUrl.getHost
      val port = schemaUrl.getPort
      val file = schemaUrl.getFile

      if (schemaUrl.getHost.nonEmpty) {
        if (schemaUrl.getPort != -1) {
          createUrl(s"$protocol://$host:$port")
        } else {
          createUrl(s"$protocol://$host")
        }
      } else {
        createUrl(s"$protocol://${file.substring(0, file.lastIndexOf("/"))}")
      }
    })
  }

  /**
    * Resolve the given fragments relatively against the base URL.
    *
    * @param relativeFragments a fragments to be resolved
    * @param scope the resolution scope
    * @return the resolved schema
    */
  private def resolveRelative(relativeFragments: String, scope: ResolutionScope): Option[SchemaType] = {
    val hashIdx = relativeFragments.indexOf("#")
    val hasHash = hashIdx != -1
    val documentName = if (hasHash) relativeFragments.substring(0, hashIdx) else relativeFragments
    for {
      normalized <- scope.id.map(id => normalize(documentName, scope))
      fetchedSchema   <- fetchSchema(createUrl(normalized))
      res <- resolveRefIfAny(scope)(fetchedSchema)
      resolved <- resolve(res,
        if (hasHash) relativeFragments.substring(hashIdx) else "#",
        scope.copy(documentRoot = res)
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
