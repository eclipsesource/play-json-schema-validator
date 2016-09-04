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
      val updatedId = refTypeClass.findScopeRefinement(a).map(id => Refs.normalize(id, scope.id, Some(resolverFactory)))
      // puts the unresolved document into the cache
      updatedId.foreach(id => cache = cache.add(id)(a))
      scope.copy(id = updatedId)
    case other => scope
  }

  private[schema] def resolveSchema(url: String, scope: GenResolutionScope[A]): Either[ValidationError, A] =
    resolve(scope.documentRoot, Ref(url), scope).right.map(_.resolved)

  private[schema] def resolve(ref: Ref, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] =
    resolve(scope.documentRoot, ref, scope)

  private[schema] def resolve(current: A, ref: Ref, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {

    def hasRef(obj: A): Boolean =
      refTypeClass.findRef(obj).fold(false) { case ref => !scope.hasBeenVisited(ref) }

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope, current)

    val result: Either[ValidationError, ResolvedResult[A]] = (current, ref) match {

      // current schema id is part of to be resolved id
      case (_, _) if refTypeClass.findScopeRefinement(current).exists(id => ref.startsWith(id)) =>
        matchAnchorIdAndResolve(current, ref, scope)

      // if current instance has a ref, resolve that one first
      case (_, _) if hasRef(current) =>
        val Some(ref) = refTypeClass.findRef(current)
        // TODO should we check whether we need to update the document root in case current == documentRoot?
        resolve(current, ref, updatedScope.addVisited(ref)).right flatMap continueResolving(ref)

      case (_, _) if  ref.isAbsolute =>
        // could be an id
        resolveAnchorId(ref, updatedScope, current) orElse
          // protocol-ful relative refs; relative refs with custom schemes will be recognized as absolute
          resolveRelative(ref, scope ) orElse
          resolveDocument(ref, updatedScope)

      // since the cache may contain unresolved refs, this must come after the previous case
      case (_, _) if cache.contains(ref) =>
        Right(ResolvedResult(cache(ref), scope))

      // resolve root only
      case (_, Refs.`#`) =>
        Right(ResolvedResult(updatedScope.documentRoot, updatedScope))

      // resolve root and continue with the rest of the ref
      case (_, _) if ref.isFragment && !ref.isAnchor =>
        resolve(updatedScope.documentRoot, ref.dropHashAtStart, updatedScope.copy(id = updatedScope.id.map(_.withHashAtEnd)))

      case (_, _) =>
        resolveFragments(toSegments(ref), updatedScope, current) orElse
          resolveRelative(ref, updatedScope) orElse
          resolveAnchorId(ref, updatedScope, current) orElse
          resolveWithRelativeUrlHandlers(ref, scope)
    }

    result match {
      case Right(resolvedResult@ResolvedResult(r, s)) =>
        refTypeClass.findRef(r).fold(result)(foundRef => continueResolving(foundRef)(resolvedResult))
      case other => Left(resolutionFailure(ref)(updatedScope))
    }
  }

  // protocol-less relative URL resolution
  private def resolveWithRelativeUrlHandlers(ref: Ref, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    val normalized = Refs.normalize(ref, scope.id, Some(resolverFactory))
    val foundResult = resolverFactory.relativeUrlHandlers.map { case (_, handler) =>
      val url  = new URL(null, normalized.value, handler)
      fetch(url, scope)
    }.find(_.isRight)

    foundResult
      .map(a => a.right.map(result => ResolvedResult(result, scope)))
      .getOrElse(Left(resolutionFailure(ref)(scope)))
  }

  private def continueResolving(ref: Ref)(resolvedResult: ResolvedResult[A]) = {
    resolve(resolvedResult.resolved, ref, resolvedResult.scope)
  }

  private def matchAnchorIdAndResolve(current: A, ref: Ref, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    val foundId = refTypeClass.findScopeRefinement(current)
    val normalized = Refs.normalize(ref, foundId, Some(resolverFactory))
    foundId.fold[Either[ValidationError, ResolvedResult[A]]] {
      Left(ValidationError(s"Resolution scope ID must not be empty."))
    } { id =>
      if (ref == id) Right(ResolvedResult(current, scope))
      else {
        val rest = if (id.endsWith("#")) normalized.drop(id.length - 1) else normalized.drop(id.length)
        resolve(current, rest, scope)
      }
    }
  }

  private def resolveAnchorId(ref: Ref, scope: GenResolutionScope[A], a: A): Either[ValidationError, ResolvedResult[A]] = {
    val normalized = Refs.normalize(ref, scope.id, Some(resolverFactory))
    val knownAnchors = refTypeClass.anchorsOf(a)
    knownAnchors.get(normalized)
      .map(r => Right(ResolvedResult(r, scope))).getOrElse(Left(resolutionFailure(normalized)(scope)))
  }

  // TODO: change error reporting format
  private def resolutionFailure(ref: Ref)(scope: GenResolutionScope[A]): ValidationError =
    ValidationError(s"Could not resolve ref ${ref.value}")

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
              resolve(r, Ref(rest.mkString("/")), scope.copy(
                schemaPath = scope.schemaPath.compose(JsPath \ fragment),
                instancePath = scope.instancePath.compose(JsPath \ fragment)
              ))
          }
        }
    }
  }

  private def createUrl(ref: Ref): Either[ValidationError, URL] = {
    // use handlers for protocol-ful absolute refs or fall back to default behaviour via null
    val handler: URLStreamHandler = ref.scheme.map(resolverFactory.createURLStreamHandler).orNull
    val triedUrl = Try { new URL(null, ref.value, handler) }
    triedUrl match {
      case Success(url) => Right(url)
      case _            => Left(ValidationError(s"Could not resolve ref ${ref.value}"))
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

    val ref = Ref(url.toString)

    def readSource(source: Source): Either[ValidationError, A] = {
      using(source) { src =>
        val resolved = for {
          json <- parseJson(source).right
          resolvedSchema <- readJson(json).right
        } yield resolvedSchema
        resolved.right.map { res =>
          cache.add(Refs.normalize(ref, scope.id, Some(resolverFactory)))(res)
          res
        }
      }
    }

    cache.get(ref) match {
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
  private def resolveRelative(ref: Ref, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    // pass in resolver factory to recognize custom schemes
    val normalized = Refs.normalize(ref, scope.id, Some(resolverFactory))
    for {
      url           <- createUrl(normalized).right
      fetchedSchema <- fetch(url, scope).right
      result        <- resolve(fetchedSchema,
        ref.fragments.getOrElse(Refs.`#`),
        scope.copy(documentRoot = fetchedSchema)
      ).right
    } yield result
  }

  private def resolveDocument(ref: Ref, scope: GenResolutionScope[A]) = {
    for {
      documentUrl <- createUrl(ref.documentName).right
      instance    <- fetch(documentUrl, scope).right
      result      <- ref.fragments
        .map(frags => resolve(instance, frags, scope.copy(documentRoot = instance)))
        .getOrElse(Right(ResolvedResult(instance, scope))).right
    } yield result
  }

  /**
    * Split the given ref into single segments.
    * Only the fragments of the given ref will be considered.
    *
    * @param ref the reference that should be split up into single segments
    * @return a list containing all the segments
    */
  private[schema] def toSegments(ref: Ref): List[String] = {

    def escape(s: String): String =
      URLDecoder.decode(s, "UTF-8")
        .replace("~1", "/")
        .replace("~0", "~")

    ref.fragments.map(_.value)
      .getOrElse(ref.value)
      .split("/").toList
      .map(escape)
  }
}
