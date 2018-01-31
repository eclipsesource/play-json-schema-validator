package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder, URLStreamHandler}

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import com.osinka.i18n.{Lang, Messages}
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
    * Update the resolution scope based on the current element.
    *
    * @param scope the current resolution scope
    * @param a the value that might contain scope refinements
    * @return the updated scope, if the given value contain a scope refinement, otherwise
    *         the not updated scope
    */
  private[schema] def updateResolutionScope(scope: GenResolutionScope[A], a: A): GenResolutionScope[A] = a match {
    case _ if refTypeClass.refinesScope(a) =>
      val updatedId = refTypeClass.findScopeRefinement(a).map(
        id => Refs.mergeRefs(id, scope.id, Some(resolverFactory))
      )
      // cache schema for later retrieval
      updatedId.foreach(id => cache = cache.add(id)(a))
      scope.copy(id = updatedId)
    case _ => scope
  }

  /**
    * Resolve the given ref against the current schema. The current
    * schema must not contain
    *
    * @param current the current schema to resolve the ref against.
    * @param ref the ref to be resolved
    * @param scope the current resolution scope
    * @param lang the language to be used
    * @return the resolved schema together with the scope.
    */
  private[schema] def resolve(current: A, ref: Ref, scope: GenResolutionScope[A])
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult[A]] = {

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope.copy(depth = scope.depth + 1), current)

    if (scope.depth >= MaxDepth) {
      JsonValidationError(Messages("err.max.depth")).left
    } else {
      val result: \/[JsonValidationError, ResolvedResult[A]] = ref match {

        case l@LocalRef(_) =>
          resolveLocal(splitFragment(l), scope, current)

          // check if ref is contained in knownSchemas
          // can also apply to relative URLs if not resolution scope is available
        case r if scope.knownSchemas.keySet.contains(r) =>
          ResolvedResult(scope.knownSchemas(r).asInstanceOf[A], scope).right[JsonValidationError]

        // check if any prefix of ref matches current element
        case a@AbsoluteRef(absoluteRef)  =>
          val currentResolutionScope = refTypeClass.findScopeRefinement(current)

          currentResolutionScope.collectFirst { case id if absoluteRef.startsWith(id.value) =>
            absoluteRef.drop(id.value.length - 1)
          }
            .map(remaining => resolve(current, Ref(remaining), updatedScope))
            .getOrElse(resolveAbsolute(a, updatedScope))

        case r@RelativeRef(_) =>
          resolveRelative(r, updatedScope, current)
      }

      result match {
        case \/-(resolvedResult@ResolvedResult(resolved, _)) =>
          // if resolved result is ref, keep on going
          refTypeClass.findRef(resolved)
            .fold(result)(foundRef =>
              resolve(resolvedResult.resolved, foundRef, resolvedResult.scope)
            )
        case _ => resolutionFailure(ref)(updatedScope).left
      }
    }
  }

  private[schema] def resolutionFailure(ref: Ref)(scope: GenResolutionScope[A])
                               (implicit lang: Lang): JsonValidationError =
    JsonValidationError(Messages("err.unresolved.ref", ref.value))

  private def resolveRelative(ref: RelativeRef, scope: GenResolutionScope[A], instance: A)
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult[A]] = {
    Refs.mergeRefs(ref, scope.id) match {
      case a@AbsoluteRef(_) =>
        resolve(instance, a, scope)
      case r@RelativeRef(relativeRef) =>
        val (file, localRef) = relativeRef.splitAt(relativeRef.indexOf("#"))
        val result = for {
          schema <- cache.get(file)
        } yield resolve(
          schema,
          LocalRef(localRef),
          scope.copy(
            documentRoot = schema,
            id = updateResolutionScope(scope, schema).id,
            origin = Some(scope.schemaPath),
            schemaUri = Some(file)
          )
        )
        result.getOrElse(resolutionFailure(r)(scope).left)
    }
  }

  private[schema] def resolveLocal(schemaPath: List[String], scope: GenResolutionScope[A], instance: A)
                                  (implicit lang: Lang): \/[JsonValidationError, ResolvedResult[A]] = {

    (schemaPath, instance) match {
      case (Nil, _) =>
        \/.fromEither(
          refTypeClass.resolve(instance, "")
            .map(resolved => ResolvedResult(resolved, scope))
        ) orElse ResolvedResult(instance, scope).right
      case (schemaProp :: rest, resolvable) =>
        schemaProp match {
          case "#" => resolveLocal(rest, scope.copy(schemaPath = JsPath \ "#"), scope.documentRoot)
          case _ => \/.fromEither(refTypeClass.resolve(resolvable, schemaProp)).flatMap { r =>
            resolveLocal(
              rest,
              updateResolutionScope(scope, r).copy(
                schemaPath = scope.schemaPath.compose(JsPath \ schemaProp)
              ),
              r
            )
          }
        }
    }
  }

  private def createUrl(ref: Ref): \/[JsonValidationError, URL] = {
    // use handlers for protocol-ful absolute refs or fall back to default behaviour via null
    val handler: URLStreamHandler = ref.scheme.map(resolverFactory.createURLStreamHandler).orNull
    val triedUrl = Try { new URL(null, ref.value, handler) }
    triedUrl match {
      case Success(url) => url.right
      case _            => JsonValidationError(s"Could not resolve ref ${ref.value}").left
    }
  }

  /**
    * Fetch the instance located at the given URL and eventually cache it as well.
    *
    * @param url the URL to fetch from
    * @param scope the current resolution scope
    * @return the fetched instance, if any
    */
  private def fetch(url: URL, scope: GenResolutionScope[A])(implicit lang: Lang): \/[JsonValidationError, A] = {

    def parseJson(source: Source): \/[JsonValidationError, JsValue] = \/.fromEither(Try {
      Json.parse(source.getLines().mkString)
    }.toJsonEither)

    def readJson(json: JsValue): \/[JsonValidationError, A] = \/.fromEither(Json.fromJson[A](json).asEither)
      .leftMap(errors =>
        JsonValidationError(Messages("err.parse.json"), JsError.toJson(errors))
      )

    val ref = Ref(url.toString)

    def readSource(source: Source): \/[JsonValidationError, A] = {
      using(source) { src =>
        val resolved = for {
          json <- parseJson(src)
          resolvedSchema <- readJson(json)
        } yield resolvedSchema
        resolved.foreach { res =>
          cache = cache.add(Refs.mergeRefs(ref, scope.id, Some(resolverFactory)))(res)
        }
        resolved
      }
    }

    cache.get(ref.value) match {
      case Some(a) => a.right
      case _ => for {
        source <- \/.fromEither(Try { Source.fromURL(url) }.toJsonEither)
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
  private def resolveAbsolute(ref: AbsoluteRef, scope: GenResolutionScope[A])
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult[A]] = {

    for {
      url <- createUrl(ref.documentName)
      fetchedSchema <- fetch(url, scope)
      result <- resolve(fetchedSchema,
        ref.fragments.getOrElse(Refs.`#`),
        scope.copy(
          id = updateResolutionScope(scope, fetchedSchema).id,
          documentRoot = fetchedSchema,
          origin = Some(scope.schemaPath),
          schemaUri = Some(url.toString)
        )
      )
    } yield result
  }

  /**
    * Split the given ref into single segments.
    * Only the fragments of the given ref will be considered.
    *
    * @param ref the reference that should be split up into single segments
    * @return a list containing all the segments
    */
  private def splitFragment(ref: Ref): List[String] = {

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
