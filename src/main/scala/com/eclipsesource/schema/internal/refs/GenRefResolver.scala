package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder, URLStreamHandler}

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.io.Source
import scala.util.{Success, Try}

case class ResolvedResult[A](resolved: A, scope: GenResolutionScope[A])

/**
  * Generic reference resolver.
  *
  * @tparam A the type that is ought to contain references
  */
case class GenRefResolver[A : CanHaveRef : Reads]
(
  resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory(),
  // FIXME: try to avoid var here
  private[schema] var cache: DocumentCache[A] = DocumentCache[A]()
) {

  val refTypeClass = implicitly[CanHaveRef[A]]

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
      val updatedId = refTypeClass.findScopeRefinement(a).map(id => Pointers.normalize(id, scope.id, Some(resolverFactory)))
      // puts the unresolved document into the cache
      updatedId.foreach(id => cache = cache.add(id)(a))
      scope.copy(id = updatedId)
    case other => scope
  }

  private[schema] def resolveSchema(url: String, scope: GenResolutionScope[A]): Either[ValidationError, A] =
    resolve(scope.documentRoot, Pointer(url), scope).right.map(_.resolved)

  private[schema] def resolve(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] =
    resolve(scope.documentRoot, pointer, scope)

  private[schema] def resolve(current: A, pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {

    def hasRef(obj: A): Boolean =
      refTypeClass.findRef(obj).fold(false) { case ref => !scope.hasBeenVisited(ref) }

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope, current)

    val result: Either[ValidationError, ResolvedResult[A]] = (current, pointer) match {

      // current schema id is part of to be resolved id
      case (_, _) if refTypeClass.findScopeRefinement(current).exists(id => pointer.startsWith(id)) =>
        matchAnchorIdAndResolve(current, pointer, scope)

      // if current instance has a ref, resolve that one first
      case (_, _) if hasRef(current) =>
        val Some(ref) = refTypeClass.findRef(current)
        // TODO should we check whether we need to update the document root in case current == documentRoot?
        resolve(current, ref, updatedScope.addVisited(ref)).right flatMap continueResolving(pointer)

      case (_, _) if  pointer.isAbsolute =>
        // could be an id
        resolveAnchorId(pointer, updatedScope, current) orElse
          // protocol-ful relative refs; relative refs with custom schemes will be recognized as absolute
          resolveRelative(pointer, scope ) orElse
          resolveDocument(pointer, updatedScope)

      // since the cache may contain unresolved refs, this must come after the previous case
      case (_, _) if cache.contains(pointer) =>
        Right(ResolvedResult(cache(pointer), scope))

      // resolve root only
      case (_, Pointers.`#`) =>
        Right(ResolvedResult(updatedScope.documentRoot, updatedScope))

      // resolve root and continue with the rest of the pointer
      case (_, _) if pointer.isFragment && !pointer.isAnchor =>
        resolve(updatedScope.documentRoot, pointer.dropHashAtStart, updatedScope.copy(id = updatedScope.id.map(_.withHashAtEnd)))

      case (_, _) =>
        resolveFragments(toSegments(pointer), updatedScope, current) orElse
          resolveRelative(pointer, updatedScope) orElse
          resolveAnchorId(pointer, updatedScope, current) orElse
          resolveWithRelativeUrlHandlers(pointer, scope)
    }

    result match {
      case Right(resolvedResult@ResolvedResult(r, s)) =>
        refTypeClass.findRef(r).fold(result)(foundRef => continueResolving(foundRef)(resolvedResult))
      case other => Left(resolutionFailure(pointer)(updatedScope))
    }
  }

  // protocol-less relative URL resolution
  private def resolveWithRelativeUrlHandlers(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    val normalized = Pointers.normalize(pointer, scope.id, Some(resolverFactory))
    val foundResult = resolverFactory.relativeUrlHandlers.map { case (_, handler) =>
      val url  = new URL(null, normalized.value, handler)
      fetch(url, scope)
    }.find(_.isRight)

    foundResult
      .map(a => a.right.map(result => ResolvedResult(result, scope)))
      .getOrElse(Left(resolutionFailure(pointer)(scope)))
  }

  private def continueResolving(pointer: Pointer)(resolvedResult: ResolvedResult[A]) = {
    resolve(resolvedResult.resolved, pointer, resolvedResult.scope)
  }

  private def matchAnchorIdAndResolve(current: A, pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    val foundId = refTypeClass.findScopeRefinement(current)
    val normalized = Pointers.normalize(pointer, foundId, Some(resolverFactory))
    foundId.fold[Either[ValidationError, ResolvedResult[A]]] {
      Left(ValidationError(s"Resolution scope ID must not be empty."))
    } { id =>
      if (pointer == id) Right(ResolvedResult(current, scope))
      else {
        val rest = if (id.endsWith("#")) normalized.drop(id.length - 1) else normalized.drop(id.length)
        resolve(current, rest, scope)
      }
    }
  }

  private def resolveAnchorId(pointer: Pointer, scope: GenResolutionScope[A], a: A): Either[ValidationError, ResolvedResult[A]] = {
    val normalized = Pointers.normalize(pointer, scope.id, Some(resolverFactory))
    val knownAnchors = refTypeClass.anchorsOf(a)
    knownAnchors.get(normalized)
      .map(r => Right(ResolvedResult(r, scope))).getOrElse(Left(resolutionFailure(normalized)(scope)))
  }

  // TODO: change error reporting format
  private def resolutionFailure(pointer: Pointer)(scope: GenResolutionScope[A]): ValidationError =
    ValidationError(s"Could not resolve ref ${pointer.value}")

  /**
    * Resolve a given list of fragments against a given schema.
    *
    * @param fragments a list of single fragments to be resolved
    * @param scope the initial resolution scope
    * @param instance the instance which the fragments are to be resolved against
    * @return the resolved result, if any
    */
  private def resolveFragments(fragments: List[String], scope: GenResolutionScope[A], instance: A): Either[ValidationError, ResolvedResult[A]] = {
    (fragments, instance) match {
      case (Nil, result) => Right(ResolvedResult(result, scope))
      case (fragment :: rest, resolvable) =>
        refTypeClass.resolve(resolvable, fragment).right.flatMap { r =>
          rest match {
            case Nil =>
              Right(ResolvedResult(r, scope.copy(
                schemaPath = scope.schemaPath.compose(JsPath \ fragment),
                instancePath = scope.instancePath.compose(JsPath \ fragment)
              )))
            case _ =>
              resolve(r, Pointer(rest.mkString("/")), scope.copy(
                schemaPath = scope.schemaPath.compose(JsPath \ fragment),
                instancePath = scope.instancePath.compose(JsPath \ fragment)
              ))
          }
        }
    }
  }

  private def createUrl(pointer: Pointer): Either[ValidationError, URL] = {
    // use handlers for protocol-ful absolute refs or fall back to default behaviour via null
    val handler: URLStreamHandler = pointer.scheme.map(resolverFactory.createURLStreamHandler).orNull
    val triedUrl = Try { new URL(null, pointer.value, handler) }
    triedUrl match {
      case Success(url) => Right(url)
      case _            => Left(ValidationError(s"Could not resolve ref ${pointer.value}"))
    }
  }

  /**
    * Fetch the instance located at the given URL and eventually cache it as well.
    *
    * @param url the URL to fetch from
    * @param scope the current resolution scope
    * @return the fetched instance, if any
    */
  private[schema] def fetch(url: URL, scope: GenResolutionScope[A]): Either[ValidationError, A] = {

    def parseJson(source: Source): Either[ValidationError, JsValue] = Try {
      Json.parse(source.getLines().mkString)
    }.toEither

    def readJson(json: JsValue): Either[ValidationError, A] = Json.fromJson[A](json).asEither.left.map(errors =>
        ValidationError("Could not parse JSON", JsError.toJson(errors))
      )

    val pointer = Pointer(url.toString)

    def readSource(source: Source): Either[ValidationError, A] = {
      using(source) { src =>
        val resolved = for {
          json <- parseJson(source).right
          resolvedSchema <- readJson(json).right
        } yield resolvedSchema
        resolved.right.map { res =>
          cache.add(Pointers.normalize(pointer, scope.id, Some(resolverFactory)))(res)
          res
        }
      }
    }

    cache.get(pointer) match {
      case cached@Some(a) => Right(a)
      case otherwise => for {
          source <- Try { Source.fromURL(url) }.toEither.right
          read <- readSource(source).right
        } yield read
    }
  }


  /**
    * Resolve the given ref relatively against the base URL.
    *
    * @param ref the ref to be resolved
    * @param scope the resolution scope
    * @return the resolved schema
    */
  private def resolveRelative(ref: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    // pass in resolver factory to recognize custom schemes
    val normalized = Pointers.normalize(ref, scope.id, Some(resolverFactory))
    for {
      url           <- createUrl(normalized).right
      fetchedSchema <- fetch(url, scope).right
      result        <- resolve(fetchedSchema,
        ref.fragments.getOrElse(Pointers.`#`),
        scope.copy(documentRoot = fetchedSchema)
      ).right
    } yield result
  }

  private def resolveDocument(pointer: Pointer, scope: GenResolutionScope[A]) = {
    for {
      documentUrl <- createUrl(pointer.documentName).right
      instance    <- fetch(documentUrl, scope).right
      result      <- pointer.fragments
        .map(frags => resolve(instance, frags, scope.copy(documentRoot = instance)))
        .getOrElse(Right(ResolvedResult(instance, scope))).right
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
  private[schema] def toSegments(pointer: Pointer): List[String] = {

    def escape(s: String): String =
      URLDecoder.decode(s, "UTF-8")
        .replace("~1", "/")
        .replace("~0", "~")

    pointer.fragments.map(_.value)
      .getOrElse(pointer.value)
      .split("/").toList
      .map(escape)
  }
}
