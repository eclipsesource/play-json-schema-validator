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

  def anchors(a: A): Map[Pointer, A]

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
      val updatedId = refTypeClass.findScopeRefinement(a).map(id => Pointers.normalize(id, scope.id))
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
      refTypeClass.findRef(obj).fold(false) { case (_, refValue) => !scope.hasBeenVisited(refValue) }

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope, current)

    val result: Either[ValidationError, ResolvedResult[A]] = (current, pointer) match {

      // current schema id is part of to be resolved id
      case (_, _) if refTypeClass.findScopeRefinement(current).exists(pointer.startsWith) =>
        matchAnchorId(current, pointer, scope)

      // if current instance has a ref, resolve that one first
      case (_, _) if hasRef(current) =>
        val Some((_, ref)) = refTypeClass.findRef(current)
        resolve(current, Pointer(ref), updatedScope.addVisited(ref)).right.flatMap {
          case ResolvedResult(r, s) =>
            // TODO should we check whether we need to update the document root in case current == documentRoot?
            resolve(r, pointer, s)
        }

      case (_, _) if  pointer.isAbsolute =>
        // could be an id
        resolveId(pointer, updatedScope, current) orElse {
          // update document root and continue resolving with fragments part only, if applicable
          for {
            instance <- fetchDocument(pointer, scope).right
            result <- pointer.fragments
              .map(frags => resolve(instance, frags, scope.copy(documentRoot = instance)))
              .getOrElse(Right(ResolvedResult(instance, scope))).right
          } yield result
        }

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
        resolveFragments(splitFragment(pointer), updatedScope, current) orElse
          resolveRelative(pointer, updatedScope) orElse
          resolveId(pointer, updatedScope, current)
            .left.map(_ => resolutionFailure(pointer)(updatedScope))
    }

    result match {
      case Right(ResolvedResult(r, s)) =>
        refTypeClass.findRef(r).fold(result) { case (_, refValue) => resolve(Pointer(refValue), s) }
      case other => other
    }
  }

  private def matchAnchorId(current: A, pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {
    val foundId = refTypeClass.findScopeRefinement(current)
    val n = Pointers.normalize(pointer, foundId)
    foundId.fold[Either[ValidationError, ResolvedResult[A]]] {
      Left(ValidationError(s"Resolution scope ID must not be empty."))
    } { id =>
      if (pointer.value == id.value) Right(ResolvedResult(current, scope))
      else {
        val rest = if (id.endsWith("#")) n.drop(id.length - 1) else n.drop(id.length)
        resolve(current, rest, scope)
      }
    }
  }

  private def resolveId(pointer: Pointer, scope: GenResolutionScope[A], a: A): Either[ValidationError, ResolvedResult[A]] = {
    val normalized = Pointers.normalize(pointer, scope.id)
    val knownSubs = refTypeClass.anchors(a)
    knownSubs.get(normalized)
      .map(r => Right(ResolvedResult(r, scope))).getOrElse(Left(resolutionFailure(pointer)(scope)))
  }

  private def fetchDocument(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, A] = {
    // check if we already resolved the document
    cache.get(pointer.documentName).fold {
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
    ValidationError(s"Could not resolve ref ${pointer.withHashAtStart.value}")
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
      case _            => Left(ValidationError(s"Could not resolve ref ${pointer.value}"))
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

  /**
    * Fetch the instance located at the given URL and eventually cache it as well.
    *
    * @param url the URL at which the instance is expected
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

    Try { Source.fromURL(url) }
      .toEither
      .right
      .flatMap( using(_) { source =>
        val pointer = Pointer(url.toString)
        cache.get(pointer) match {
          case cached@Some(a) => Right(a)
          case otherwise =>
            val resolved = for {
              json <- parseJson(source).right
              resolvedSchema <- readJson(json).right
            } yield resolvedSchema
            resolved.right.map { res =>
              cache.add(Pointers.normalize(pointer, scope.id))(res)
              res
            }
        }
      }
      )
  }

  /**
    * Resolve the given fragments relatively against the base URL.
    *
    * @param ref fragments to be resolved
    * @param scope the resolution scope
    * @return the resolved schema
    */
  private def resolveRelative(ref: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = {

    val normalized = Pointers.normalize(ref, scope.id)

    for {
      url           <- (createUrl(normalized) orElse createCustomRelativeUrl(ref, scope)).right
      fetchedSchema <- fetch(url, scope).right
      result        <- resolve(fetchedSchema,
        ref.fragments.getOrElse(Pointers.`#`),
        scope.copy(documentRoot = fetchedSchema)
      ).right
    } yield result
  }

  private def createCustomRelativeUrl(ref: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, URL] = {
    val maybeHandler = resolverFactory.relativeUrlHandlers.find {
      case handler =>
        // use custom protocol in order to create a valid URL
        val url = new URL(null, s"play-validator://${ref.value}", handler)
        fetch(url, scope).isRight
    }
    maybeHandler
      .map(h => Right(new URL(null, s"play-validator://${ref.value}", h)))
      .getOrElse(Left(resolutionFailure(ref)(scope)))
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

    pointer.fragments.map(_.value)
      .getOrElse(pointer.value)
      .split("/").toList
      .map(escape)
  }
}
