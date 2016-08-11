package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder}

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.io.Source
import scala.util.{Success, Try}

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
    * Tries to find an id field which refines the resolution scope.
    *
    * @param a the instance to be checked
    * @return true, if the given instance has an id field, false otherwise
    */
  def findScopeRefinement(a: A): Option[Pointer]

  /**
    * Tries to find a resolvable instance within the given value.
    *
    * @param a the value
    * @return an Option containing the field name and value, if any ref has been found
    */
  def findRef(a: A): Option[(String, String)]
}

case class ResolvedResult[A](resolved: A, scope: GenResolutionScope[A])

/**
  * Generic reference resolver.
  *
  * @tparam A the type that is ought to contain references
  */
case class GenRefResolver[A : CanHaveRef : Reads]
(
  resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory(),
  private[schema] var cache: SchemaCache[A] = SchemaCache[A]()
) {

  val refTypeClass = implicitly[CanHaveRef[A]]

  /**
    * Normalizes a pointer URL, i.e. the outcome of this method
    * is an always absolute URL, even if the given pointer
    * is relative.
    *
    * @param pointer the URL to be normalized
    * @param scope the current resolution scope
    * @return an absolute URL
    */
  private[schema] def normalize(pointer: Pointer, scope: GenResolutionScope[A]): Pointer = {

    import Pointers._

    pointer match {

      // pointer starts with #, merge with scope id, possibly dropping a #
      //
      case _ if pointer.isFragment =>
        scope.id.map(_.dropRightHashIfAny.append(pointer)).getOrElse(pointer)

      // make sure we have a # at the end, if applicable, that is
      // the pointer has no # and also does not end with a /
      case _ if pointer.isAbsolute =>
        pointer.withHashAtEnd

      // append pointer to scope id in case the latter ends with a /
      case _ if scope.id.exists(_.isSegment) =>
        scope.id.map(_.append(pointer)).getOrElse(pointer)

      // scope id already has fragment and pointer does not
      // http://x.y.z/schema.json# and definitions/some/prop should result
      // in http://x.y.z/schema.json#/definitions/some/prop
      // TODO: currently pointer can never start with a / since we drop that in resolve already
      // TODO: but this method should be more robust
      case _ if scope.id.exists(_.hasFragment) && pointer.hasSegment && !pointer.hasFragment =>
        scope.id.map(_.append(pointer.prepend(`/`))).getOrElse(pointer)

      // if all cases failed, try to create a relative reference, e.g.
      // if given http://x.y.z/schema.json# and some.json#/definitions/prop
      // we want the result to be http://x.y.z/some.json#/definitions/prop
      case other =>
        val baseUrl = findBaseUrl(scope.id)
        baseUrl.map(url =>
          if (url.isSegment) pointer.withHashAtEnd.prepend(url)
          else pointer.withHashAtEnd.prepend(url.append(`/`))
        ).getOrElse(pointer)
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
  private[schema] def updateResolutionScope(scope: GenResolutionScope[A], a: A): GenResolutionScope[A] = a match {
    case _ if refTypeClass.refinesScope(a) =>
      val updatedId = refTypeClass.findScopeRefinement(a).map(id => normalize(id, scope))
      updatedId.foreach(id => cache = cache.addId(id)(a))
      scope.copy(id = updatedId)
    case other => scope
  }

  private[schema] def resolveSchema(url: String, scope: GenResolutionScope[A]): Either[ValidationError, A] =
    resolve(scope.documentRoot, Pointer(url), scope).right.map(_.resolved)

  private[schema] def resolve(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] =
    resolve(scope.documentRoot, pointer, scope)

  private[schema] def resolve(current: A, pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {

    def hasRef(obj: A): Boolean =
      refTypeClass.findRef(obj).fold(false) { case (_, refValue) => !scope.hasBeenVisited(refValue) }

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope, current)

    val result: Either[ValidationError, ResolvedResult[A]] = (current, pointer) match {

      // if current instance has a ref, resolve that one first
      case (obj, _) if hasRef(obj) =>
        val Some((_, ref)) = refTypeClass.findRef(obj)
        resolve(obj, Pointer(ref), updatedScope.addVisited(ref)).right.flatMap {
          case ResolvedResult(r, s) => resolve(r, pointer, s)
        }

      case (_, _) if  pointer.isAbsolute =>
        // update document root and continue resolving with fragments part only, if applicable
        for {
          instance <- fetchInstance(pointer, scope).right
          result   <- pointer.fragments
            .map(frags => resolve(instance, frags, scope.copy(documentRoot = instance)))
            .getOrElse(Right(ResolvedResult(instance, scope))).right
        } yield result

      // resolve root only
      case (_, Pointers.`#`) =>
        Right(ResolvedResult(updatedScope.documentRoot, updatedScope))

      // resolve root and continue with the rest of the pointer  
      case (_, _) if pointer.isFragment =>
        resolve(updatedScope.documentRoot, pointer.dropHashAtBeginning, updatedScope.copy(id = updatedScope.id.map(_.withHashAtEnd)))


      case (_, _) if cache.contains(pointer) =>
        Right(ResolvedResult(cache.idMapping(pointer.value), scope))

      case (container, _) =>
        resolveFragments(splitFragment(pointer), updatedScope, container) orElse
          resolveRelative(pointer, updatedScope) orElse
          resolveDefinition(pointer, updatedScope)
            .left.map(_ => resolutionFailure(pointer)(updatedScope))
    }

    result match {
      case Right(ResolvedResult(r, s)) =>
        refTypeClass.findRef(r).fold(result) { case (_, refValue) => resolve(Pointer(refValue), s) }
      case other => other
    }
  }

  private def fetchInstance(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, A] = {
    // check if we already resolved the document
    cache.getId(pointer.documentName).fold {
      // we didn't, so let's fetch the document
      // fetch will take care of putting the document into the cache
      for {
        url <- createUrl(pointer).right
        fetchedSchema <- fetch(url, scope).right
      } yield fetchedSchema
    } {
      // use cached instance
      Right(_)
    }
  }

  // TODO: change error reporting format
  // TODO: do not normalize, but rather introduce additional resolution scope property
  private def resolutionFailure(pointer: Pointer)(scope: GenResolutionScope[A]): ValidationError = {
    ValidationError(s"Could not resolve ref ${normalize(pointer, scope).value}")
  }

  private def resolveDefinition(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    pointer match {
      // check whether we have been here before
      case Pointer(value) if value.contains("definitions") => Left(resolutionFailure(pointer)(scope))
      case _ => resolve(scope.documentRoot, Pointer(s"#/definitions/${pointer.value}"), scope)
    }
  }

  private def createUrl(pointer: Pointer): Either[ValidationError, URL] = {
    val protocol = pointer.protocol
    val triedUrl = Try {
      protocol.map(p =>
        new URL(null, pointer.value, resolverFactory.createURLStreamHandler(p))
      ).getOrElse(new URL(pointer.value))
    }
    triedUrl match {
      case Success(url) => Right(url)
      case _            => Left(ValidationError(s"> Could not resolve ref ${pointer.value}"))
    }
  }

  /**
    * Resolve a given list of fragments against a given schema.
    *
    * @param fragments a list of single fragments to be resolved
    * @param scope the initial resolution scope
    * @param instance the instance which the fragments are to be resolved against
    * @return the resolved result, if any
    */
  private def resolveFragments(fragments: List[String], scope: GenResolutionScope[A], instance: A): Either[ValidationError, ResolvedResult[A]] = {
    val updatedScope = updateResolutionScope(scope, instance)
    (fragments, instance) match {
      case (Nil, result) => Right(ResolvedResult(result, scope))
      case (fragment :: rest, resolvable) =>
        refTypeClass.resolve(resolvable, fragment).right.flatMap { r =>
          rest match {
            case Nil => Right(ResolvedResult(r, scope.copy(
              schemaPath = scope.schemaPath.compose(JsPath \ fragment),
              instancePath = scope.instancePath.compose(JsPath \ fragment)
            )))
            case _ => resolve(r, Pointer(rest.mkString("/")), updatedScope.copy(
              schemaPath = scope.schemaPath.compose(JsPath \ fragment),
              instancePath = scope.instancePath.compose(JsPath \ fragment)
            ))
          }
        }
    }
  }

  /**
    * Fetch the instance located at the given URL and eventually cache it as well.
    *
    * @param url the URL at which the instance is expected
    * @param scope the current resolution scope
    * @return the fetched instance, if any
    */
  private[schema] def fetch(url: URL, scope: GenResolutionScope[A]): Either[ValidationError, A] = {
    Try { Source.fromURL(url) }
      .toEither
      .right
      .flatMap(source =>
        cache.getId(Pointer(url.toString)) match {
          case cached@Some(a) => Right(a)
          case otherwise =>
            val resolved = for {
              json <- Try {
                Json.parse(source.getLines().mkString)
              }.toEither.right
              resolvedSchema <- Json.fromJson[A](json).asEither.left.map(errors =>
                ValidationError("Could not parse JSON", JsError.toJson(errors))
              ).right
            } yield resolvedSchema
            resolved.right.map { res =>
              cache = cache.addId(normalize(Pointer(url.toString), scope))(res)
              res
            }
        }
      )
  }

  /**
    * Finds out the actual base URI based on a given resolution scope.
    *
    * @param scope the resolution scope from which to determine the base URL
    * @return the base URL, if any, otherwise None
    */
  private[schema] def findBaseUrl(scope: Option[Pointer]): Option[Pointer] = {

    def createBaseUrl(url: URL, protocol: String, host: String, port: Int, file: String): Option[URL] = {
      if (url.getHost.nonEmpty) {
        if (url.getPort != -1) {
          createUrl(Pointer(s"$protocol://$host:$port")).toOption
        } else {
          createUrl(Pointer(s"$protocol://$host")).toOption
        }
      } else {
        createUrl(Pointer(s"$protocol://${file.substring(0, file.lastIndexOf("/"))}")).toOption
      }
    }

    val url: Option[URL] = for {
      id       <- scope
      url      <- createUrl(id).toOption
      protocol = url.getProtocol
      host     = url.getHost
      port     = url.getPort
      file     = url.getFile
      baseUrl  <- createBaseUrl(url, protocol, host, port, file)
    } yield baseUrl

    url.map(u => Pointer(u.toString))
  }

  /**
    * Resolve the given fragments relatively against the base URL.
    *
    * @param ref fragments to be resolved
    * @param scope the resolution scope
    * @return the resolved schema
    */
  private def resolveRelative(ref: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {

    val normalized = normalize(ref, scope)

    for {
      url           <- createUrl(normalized).right
      fetchedSchema <- fetch(url, scope).right
      result        <- resolve(fetchedSchema,
        ref.fragments.getOrElse(Pointers.`#`),
        scope.copy(documentRoot = fetchedSchema)
      ).right
    } yield result
  }

  /**
    * Split the given pointer into single fragments.
    * If the given URI is an absolute path the base URI
    * will be ignored.
    *
    * @param pointer the pointer
    * @return the single fragments as a list
    */
  private[schema] def splitFragment(pointer: Pointer): List[String] = {

    def escape(s: String): String =
      URLDecoder.decode(s, "UTF-8")
        .replace("~1", "/")
        .replace("~0", "~")

    pointer.fragments.map(_.toString)
      .getOrElse(pointer.value)
      .split("/").toList
      .map(escape)
  }
}
