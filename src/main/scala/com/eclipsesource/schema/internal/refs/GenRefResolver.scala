package com.eclipsesource.schema.internal.refs

import java.net.{URL, URI, URLDecoder}
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.io.{Source, BufferedSource}
import scala.util.{Failure, Success, Try}

/**
  * A typeclass that determines whether a value of the given
  * type can contain references
  *
  * @tparam A the type that can contain references
  */
trait CanHaveRef[A] {

  /**
    * Resolve the fragment against the given value. A fragment
    * is a single identifier like a property name.
    *
    * @param a the value
    * @param fragment the fragment to be resolved
    * @return a right-based Either containg the result
    */
  def resolve(a: A, fragment: String): Either[ValidationError, A]

  /**
    * Whether the given value has an id field which can alter resolution scope.
 *
    * @param a the instance to be checked
    * @return true, if the given instance has an id field, false otherwise
    */
  def refinesScope(a: A): Boolean = findScopeRefinement(a).isDefined

  /**
    * Tries to find an id field w
    *
    * @param a the instance to be checked
    * @return true, if the given instance has an id field, false otherwise
    */
  def findScopeRefinement(a: A): Option[String]

  /**
    * Tries to find a resolvable instance within the given value.
    *
    * @param a the value
    * @return an Option containing the field name and value, if any ref has been found
    */
  def findRef(a: A): Option[(String, String)]
}

/**
  * Generic reference resolver.
  *
  * @tparam A the type that is ought to contain references
  */
case class GenRefResolver[A : CanHaveRef : Reads](resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory()) {

  val contextCache: GenGlobalContextCache[A] = GenGlobalContextCache[A]()
  val refTypeClass = implicitly[CanHaveRef[A]]

  private final val WithProtocol = "^([^:\\/?]+):.+"
  private final val ProtocolPattern = WithProtocol.r.pattern

  /**
    * Normalizes a pointer URL, i.e. the outcome of this method
    * is an always absolute URL, even if the given pointer
    * is relative.
    *
    * @param pointer the URL to be normalized
    * @param scope the current resolution scope
    * @return an absolute URL
    */
  private[schema] def normalize(pointer: String, scope: GenResolutionScope[A]): String = {

    def pathWithHash: String = if (!pointer.contains("#") && !pointer.endsWith("/")) s"$pointer#" else pointer
    def compose(scope: String) = {
      if (scope.endsWith("/"))
        scope + pathWithHash
      else scope + "/" + pathWithHash
    }
    def dropHashIfAny(scope: String) = if (scope.endsWith("#")) scope.dropRight(1) + pointer else scope
    val isAbsolute = Try { new URI(pointer) }.map(_.isAbsolute).getOrElse(false)

    pointer match {
      case _ if pointer.startsWith("#") =>
        scope.id.map(dropHashIfAny).getOrElse(pointer)
      case _ if isAbsolute =>
        pathWithHash
      case other =>
        val resolutionScope = if (scope.isRootScope) findBaseUrl(scope.id).map(_.toString) else scope.id
        resolutionScope.map(compose).getOrElse(pointer)
    }
  }

  /**
    * Find a reference in the given entity, if any and try to resolve it.
    *
    * @param scope the resolution scope
    * @param entity the entity that might contain a reference
    * @return the resolved value contained in a right-biased Either
    */

  private[schema] def resolvePointerIfAny(scope: GenResolutionScope[A])(entity: A): Either[ValidationError, A] = {
    def findRef(obj: A): Boolean = {
      val refOpt = refTypeClass.findRef(obj)
      refOpt.fold(false) { case (_, pointer) => !scope.hasBeenVisited(pointer) }
    }
    entity match {
      case obj if findRef(obj) =>
        val Some((unvisited, updatedScope)) =
          refTypeClass.findRef(obj)
            .map { case (_, pointer) => pointer -> updateResolutionScope(scope, obj).addVisited(pointer) }
        for {
          resolved  <- resolve(url = unvisited, updatedScope).right
          r <- resolvePointerIfAny(updatedScope)(resolved).right
        } yield r

      case other => Right(other)
    }
  }

  /**
    * Update the resolution scope.
    *
    * @param scope the current resolution scope
    * @param a the value that might contain scope refinements
    * @return the updated scope, if the given value contain a scope refinement, otherwise
    *         the not updated scope
    */
  def updateResolutionScope(scope: GenResolutionScope[A], a: A): GenResolutionScope[A] = a match {
    case _ if refTypeClass.refinesScope(a) =>
      val updatedScope = for {
        baseId <- scope.id
        schemaId <- refTypeClass.findScopeRefinement(a)
      } yield normalize(schemaId, scope)
      scope.copy(id = updatedScope orElse scope.id)
    case other => scope
  }

  private[schema] def resolve(url: String, scope: GenResolutionScope[A]): Either[ValidationError, A] = {
    if (url.isEmpty) {
      Right(scope.documentRoot)
    } else {
      contextCache.get(url).fold {
        for {
        // root is potentially a $ref
          root <- resolvePointerIfAny(scope)(scope.documentRoot).right
          resolved <- resolve(root, url, scope.copy(documentRoot = root)).right
        } yield {
          contextCache.add(normalize(url, scope))(resolved)
        }
      } { Right(_) }
    }
  }

  private def hasProtocol(path: String): Boolean = path.matches(WithProtocol)

  private def createUrl(pointer: String): Either[ValidationError, URL] = {
    val matcher = ProtocolPattern.matcher(pointer)
    matcher.find()
    val protocol = Try { matcher.group(1).replaceAll("[^A-Za-z]+", "") }
    val triedUrl = Try(protocol match {
      case Success(p) => new URL(null, pointer, resolverFactory.createURLStreamHandler(p))
      case Failure(_) => new URL(pointer)
    })
    triedUrl match {
      case Success(url) => Right(url)
      case _            => Left(ValidationError(s"Could not resolve ref $pointer"))
    }
  }

  private def resolve(current: A, path: String, scope: GenResolutionScope[A]): Either[ValidationError, A] = {

    (current, path) match {

      case (_, pointer) if hasProtocol(pointer) =>
        val fragmentStartIndex = pointer.indexOf("#")
        val hasHash = fragmentStartIndex > 0
        val idx = if (hasHash) fragmentStartIndex + 1 else 0

        // change document root
        def resolveWithUpdatedRoot(resolved: A): Either[ValidationError, A] = {
          if (hasHash) {
            val root = if (current == scope.documentRoot) resolved else scope.documentRoot
            resolve(resolved, "#" + pointer.substring(idx), scope.copy(documentRoot = root))
          } else {
            Right(resolved)
          }
        }

        for {
          url <- createUrl(pointer).right
          fetchedSchema <- fetch(url).right
          resolved <- resolvePointerIfAny(scope)(fetchedSchema).right
          result <- resolveWithUpdatedRoot(resolved).right
        } yield result

      case (schema, "#") => Right(scope.documentRoot)

      case (schema, ref) if ref.startsWith("#") =>
        resolve(scope.documentRoot, ref.substring(math.min(2, ref.length)), scope)

      case (container, fragments) =>
        val resolved: Either[ValidationError, A] = resolveFragments(toFragments(fragments), scope, container)
        resolved.fold(_ => resolveRelative(fragments, scope), Right(_))
      case (_, _) => Right(current)
    }
  }

  /**
    * Resolve a given list of fragments against a given schema.
    *
    * @param fragments a list of single fragments to be resolved
    * @param scope the initial resolution scope
    * @param a the instance which the fragments are to be resolved against
    * @return the resolved result, if any
    */
  private def resolveFragments(fragments: List[String], scope: GenResolutionScope[A], a: A): Either[ValidationError, A] = {
    (fragments, a) match {
      case (Nil, result) => Right(result)
      case (fragment :: rest, resolvable) =>
        for {
          resolved <- refTypeClass.resolve(resolvable, fragment).right
          res <- resolveFragments(rest,
            scope.copy(
              schemaPath = scope.schemaPath.compose(JsPath \ fragment),
              instancePath = scope.instancePath.compose(JsPath \ fragment)
            ),
            resolved
          ).right
          // res is possibly a ref
          result <- resolvePointerIfAny(scope)(res).right
        } yield result
    }
  }

  /**
    * Fetch an instance from the given URL.
    *
    * @param url the absolute URL string from which to fetch schema
    * @return the resolved instance contained in a right-biased Either
    */
  private[schema] def fetch(url: URL): Either[ValidationError, A] = {
    contextCache.get(url.toString) match {
      case cached@Some(a) => Right(a)
      case otherwise =>
        val contents: BufferedSource = Source.fromURL(url)
        val resolved = for {
          json           <- Try { Json.parse(contents.getLines().mkString) }.toEither.right
          resolvedSchema <- Json.fromJson[A](json).asEither.left.map(errors =>
            ValidationError("Could not parse JSON", JsError.toJson(errors))
          ).right
        } yield resolvedSchema
        resolved.right.map { contextCache.add(url.toString) }
    }
  }

  /**
    * Finds out the actual base URI based on a given resolution scope.
    *
    * @param scope the resolution scope from which to determine the base URL
    * @return the base URL, if any, otherwise None
    */
  private def findBaseUrl(scope: Option[String]): Option[URL] = {

    def createBaseUrl(url: URL, protocol: String, host: String, port: Int, file: String) = {
      // TODO review
      if (url.getHost.nonEmpty) {
        if (url.getPort != -1) {
          createUrl(s"$protocol://$host:$port").toOption
        } else {
          createUrl(s"$protocol://$host").toOption
        }
      } else {
        createUrl(s"$protocol://${file.substring(0, file.lastIndexOf("/"))}").toOption
      }
    }

    for {
      id       <- scope
      url      <- createUrl(id).toOption
      protocol = url.getProtocol
      host     = url.getHost
      port     = url.getPort
      file     = url.getFile
      baseUrl  <- createBaseUrl(url, protocol, host, port, file)
    } yield baseUrl
  }

  /**
    * Resolve the given fragments relatively against the base URL.
    *
    * @param relativeFragments a fragments to be resolved
    * @param scope the resolution scope
    * @return the resolved schema
    */
  private def resolveRelative(relativeFragments: String, scope: GenResolutionScope[A]): Either[ValidationError, A] = {
    val hashIdx = relativeFragments.indexOf("#")
    val hasHash = hashIdx != -1
    val documentName = if (hasHash) relativeFragments.substring(0, hashIdx) else relativeFragments
    val normalized = normalize(documentName, scope)
    for {
      url           <- createUrl(normalized).right
      fetchedSchema <- fetch(url).right
      res           <- resolvePointerIfAny(scope)(fetchedSchema).right
      resolved      <- resolve(res,
        if (hasHash) relativeFragments.substring(hashIdx) else "#",
        scope.copy(documentRoot = res)
      ).right
    } yield resolved

  }

  /**
    * Split the given URL string into single fragments.
    * If the given URI is an absolute path the base URI
    * will be ignored.
    *
    * @param url the URl to be split
    * @return the single fragments as a list
    */
  private[schema] def toFragments(url: String): List[String] = {

    def escape(s: String): String =
      URLDecoder.decode(s, "UTF-8")
        .replace("~1", "/")
        .replace("~0", "~")

    def withoutBaseUri(s: String): Option[String] = {
      s.find(_ == '#')
        .map(_ => url.substring(url.indexOf("#")))
    }

    val withoutBase = withoutBaseUri(url)
    withoutBase.getOrElse(url)
      .split("/").toList
      .map(escape)
  }
}