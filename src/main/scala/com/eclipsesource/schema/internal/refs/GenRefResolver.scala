package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder, URLStreamHandler}

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import com.osinka.i18n.{Lang, Messages}
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.io.Source
import scala.util.{Success, Try}
import scalaz.{\/, \/-}
import scalaz.syntax.either._

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

  val MaxDepth: Int = 100
  val refTypeClass: CanHaveRef[A] = implicitly[CanHaveRef[A]]

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
    case _ => scope
  }

  private[schema] def resolveSchema(url: String, scope: GenResolutionScope[A])
                                   (implicit lang: Lang = Lang.Default): Either[ValidationError, A] =
    resolve(scope.documentRoot, Ref(url), scope).map(_.resolved).toEither

  private[schema] def resolve(ref: Ref, scope: GenResolutionScope[A])
                             (implicit lang: Lang = Lang.Default): Either[ValidationError, ResolvedResult[A]] =
    resolve(scope.documentRoot, ref, scope).toEither

  private[schema] def resolve(current: A, ref: Ref, scope: GenResolutionScope[A])
                             (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {

    def hasRef(obj: A) = refTypeClass.findRef(obj).fold(false)(r => !scope.hasBeenVisited(r))

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope.copy(depth = scope.depth + 1), current)

    if (scope.depth >= MaxDepth) {
      ValidationError(Messages("err.max.depth")).left
    } else {
      val result: \/[ValidationError, ResolvedResult[A]] = (current, ref) match {

        // id in current is prefix of ref
        case (_, _) if refTypeClass.findScopeRefinement(current).exists(id => ref.startsWith(id)) =>
          matchAnchorIdAndResolve(current, ref, scope)

        // check if current instance contains the ref to be resolved
        // or whether it's another one that needs to be resolved first
        case (_, _) if hasRef(current) =>
          val Some(anotherRef) = refTypeClass.findRef(current)
          val result = resolve(current, anotherRef, updatedScope.addVisited(anotherRef))
          if (anotherRef == ref) {
            // was same ref
            result
          } else {
            // current contained another ref, keep on resolving
            result flatMap continueResolving(ref)
          }

        case (_, _) if ref.isAbsolute =>
          // could be an id
          resolveAnchorId(ref, updatedScope, current) orElse
            // protocol-ful relative refs; relative refs with custom schemes will be recognized as absolute
            resolveRef(ref, scope)

        // since the cache may contain unresolved refs, this must be handled after the previous case
        case (_, _) if cache.exists(id => ref.startsWith(id)) =>
          resolveCache(ref, updatedScope)

        // resolve root only
        case (_, Refs.`#`) =>
          ResolvedResult(
            updatedScope.documentRoot,
            updatedScope.copy(schemaPath = JsPath \ "#")
          ).right

        // resolve local root and continue with the rest of the ref
        case (_, _) if ref.isFragment && !ref.isAnchor =>
          resolve(updatedScope.documentRoot, ref.dropHashAtStart,
            updatedScope.copy(
              id = updatedScope.id,
              schemaPath = JsPath \ "#"
            )
          )

        case (_, _) =>
          resolveFragments(toSegments(ref), updatedScope, current) orElse
            resolveAnchorId(ref, updatedScope, current) orElse
            resolveRef(ref, updatedScope) orElse
            resolveWithRelativeUrlHandlers(ref, scope)
      }

      result match {
        case \/-(resolvedResult@ResolvedResult(resolved, _)) =>
          refTypeClass.findRef(resolved)
            .fold(result)(foundRef => continueResolving(foundRef)(resolvedResult))
        case _ => resolutionFailure(ref)(updatedScope).left
      }
    }
  }

  private def resolveCache(ref: Ref, updatedScope: GenResolutionScope[A])
                          (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {
    cache.mapping.find { case (id, s) => ref.value.startsWith(id) } match {
      case Some((identifier, a)) if identifier == ref.value => ResolvedResult(a, updatedScope).right
      case Some((identifier, a)) =>
        resolve(a, ref.drop(identifier.length), updatedScope.copy(
          // set document root to resolved sub-schema
          documentRoot = a,
          id = Some(Ref(identifier)),
          schemaPath = JsPath \ "#",
          origin = Some(updatedScope.schemaPath)
        ))
    }
  }

  // protocol-less relative URL resolution
  private def resolveWithRelativeUrlHandlers(ref: Ref, scope: GenResolutionScope[A])
                                            (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {
    val normalized = Refs.normalize(ref, scope.id, Some(resolverFactory))
    val foundResult = resolverFactory.relativeUrlHandlers.map { case (_, handler) =>
      val url  = new URL(null, normalized.value, handler)
      fetch(url, scope)
    }.find(_.isRight)


    foundResult.fold[\/[ValidationError, ResolvedResult[A]]](resolutionFailure(ref)(scope).left)(right =>
      right.map(result => ResolvedResult(result, scope))
    )
  }

  private def continueResolving(ref: Ref)(resolvedResult: ResolvedResult[A])
                               (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {
    resolve(resolvedResult.resolved, ref, resolvedResult.scope)
  }

  private def matchAnchorIdAndResolve(current: A, ref: Ref, scope: GenResolutionScope[A])
                                     (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {
    val foundId = refTypeClass.findScopeRefinement(current)
    val normalized = Refs.normalize(ref, foundId, Some(resolverFactory))

    foundId.fold[\/[ValidationError, ResolvedResult[A]]] {
      ValidationError(Messages("err.res.scope.id.empty")).left
    } { id =>
      if (ref == id) ResolvedResult(current, scope).right
      else {
        val rest = if (id.endsWith("#")) normalized.drop(id.length - 1) else normalized.drop(id.length)
        resolve(current, rest, scope)
      }
    }
  }

  private def resolveAnchorId(ref: Ref, scope: GenResolutionScope[A], a: A)
                             (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {
    val normalized = Refs.normalize(ref, scope.id, Some(resolverFactory))
    val knownAnchors = refTypeClass.anchorsOf(a)
    val normalizedAnchorIdMap = knownAnchors.map { case (r, s) =>
      if (!r.isAbsolute && !r.isFragment) {
        // anchor id must start with #
        Refs.normalize(r.prepend(Ref("#")), scope.id, Some(resolverFactory)) -> s
      } else {
        Refs.normalize(r, scope.id, Some(resolverFactory)) -> s
      }
    }
    normalizedAnchorIdMap.get(normalized)
      .map(r => ResolvedResult(r, scope).right).getOrElse(resolutionFailure(normalized)(scope).left)
  }

  // TODO: change error reporting format
  private def resolutionFailure(ref: Ref)(scope: GenResolutionScope[A])
                               (implicit lang: Lang): ValidationError =
    ValidationError(s"Could not resolve ref ${ref.value}")

  /**
    * Resolve a given list of fragments against a given resolved schema instance.
    *
    * @param fragments a list of single fragments to be resolved
    * @param scope the initial resolution scope
    * @param instance a resolved schema instance
    * @return the resolved result, if any
    */
  private[schema] def resolveFragments(fragments: List[String], scope: GenResolutionScope[A], instance: A)
                              (implicit lang: Lang = Lang.Default): \/[ValidationError, ResolvedResult[A]] = {

    (fragments, instance) match {
      case (Nil, _) => ResolvedResult(instance, scope).right
      case (fragment :: rest, resolvable) =>
        \/.fromEither(refTypeClass.resolve(resolvable, fragment)).flatMap { r =>
          rest match {
            case Nil =>
              ResolvedResult(r, scope.copy(
                schemaPath = scope.schemaPath.compose(JsPath \ fragment)
              )).right
            case _ =>
              resolve(r, Ref(rest.mkString("/")), scope.copy(
                schemaPath = scope.schemaPath.compose(JsPath \ fragment)
              ))
          }
        }
    }
  }

  private def createUrl(ref: Ref): \/[ValidationError, URL] = {
    // use handlers for protocol-ful absolute refs or fall back to default behaviour via null
    val handler: URLStreamHandler = ref.scheme.map(resolverFactory.createURLStreamHandler).orNull
    val triedUrl = Try { new URL(null, ref.value, handler) }
    triedUrl match {
      case Success(url) => url.right
      case _            => ValidationError(s"Could not resolve ref ${ref.value}").left
    }
  }

  /**
    * Fetch the instance located at the given URL and eventually cache it as well.
    *
    * @param url the URL to fetch from
    * @param scope the current resolution scope
    * @return the fetched instance, if any
    */
  private def fetch(url: URL, scope: GenResolutionScope[A])(implicit lang: Lang): \/[ValidationError, A] = {

    def parseJson(source: Source): \/[ValidationError, JsValue] = \/.fromEither(Try {
      Json.parse(source.getLines().mkString)
    }.toEither)

    def readJson(json: JsValue): \/[ValidationError, A] = \/.fromEither(Json.fromJson[A](json).asEither)
      .leftMap(errors =>
        ValidationError(Messages("err.parse.json"), JsError.toJson(errors))
      )

    val ref = Ref(url.toString)

    def readSource(source: Source): \/[ValidationError, A] = {
      using(source) { src =>
        val resolved = for {
          json <- parseJson(src)
          resolvedSchema <- readJson(json)
        } yield resolvedSchema
        resolved.foreach { res =>
          cache = cache.add(Refs.normalize(ref, scope.id, Some(resolverFactory)))(res)
        }
        resolved
      }
    }

    cache.get(ref) match {
      case Some(a) => a.right
      case _ => for {
        source <- \/.fromEither(Try { Source.fromURL(url) }.toEither)
        read <- readSource(source)
      } yield read
    }
  }


  /**
    * Resolve the given ref. The given ref may be relative or absolute.
    * If is relative it will be normalized against the current resolution scope.
    *
    * @param ref the ref to be resolved
    * @param scope the resolution scope that will be used for normalization
    * @return the resolved schema
    */
  private def resolveRef(ref: Ref, scope: GenResolutionScope[A])
                             (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {

    // pass in resolver factory to recognize custom schemes
    val normalized = Refs.normalize(ref, scope.id, Some(resolverFactory))
    for {
      url <- createUrl(normalized.documentName)
      fetchedSchema <- fetch(url, scope)
      result <- resolve(fetchedSchema,
        normalized.fragments.getOrElse(Refs.`#`),
        scope.copy(
          documentRoot = fetchedSchema,
          origin = Some(scope.schemaPath)
        )
      )
    } yield result
  }

  private def resolveDocument(ref: Ref, scope: GenResolutionScope[A])
                             (implicit lang: Lang): \/[ValidationError, ResolvedResult[A]] = {
    for {
      documentUrl <- createUrl(ref.documentName)
      instance    <- fetch(documentUrl, scope)
      result      <- ref.fragments
        .map(frags => resolve(instance, frags, scope.copy(documentRoot = instance)))
        .getOrElse(ResolvedResult(instance, scope.copy(schemaPath = JsPath \ "#")).right)
    } yield result
  }

  /**
    * Split the given ref into single segments.
    * Only the fragments of the given ref will be considered.
    *
    * @param ref the reference that should be split up into single segments
    * @return a list containing all the segments
    */
  private def toSegments(ref: Ref): List[String] = {

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
