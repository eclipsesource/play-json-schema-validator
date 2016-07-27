package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder}

import com.eclipsesource.schema.{Pointer, Pointers}
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.io.Source
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
case class GenRefResolver[A : CanHaveRef : Reads](resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory()) {

  val pointerToSchemaCache: PointerToSchemaCache[A] = PointerToSchemaCache[A]()
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
      case _ if pointer.isFragment =>
        scope.id.map(_.dropHashAtEnd.append(pointer)).getOrElse(pointer)
      case _ if pointer.isAbsolute =>
        pointer.withHashAtEnd
      case _ if scope.id.exists(_.isSegment) =>
        scope.id.map(_.append(pointer)).getOrElse(pointer)
      case _ =>
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
  def updateResolutionScope(scope: GenResolutionScope[A], a: A): GenResolutionScope[A] = a match {
    case _ if refTypeClass.refinesScope(a) =>
      val updatedId = refTypeClass.findScopeRefinement(a).map(id => normalize(id, scope))
      updatedId.foreach(id => pointerToSchemaCache.addId(id)(a))
      scope.copy(id = updatedId orElse scope.id)
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

      case (obj, _) if hasRef(obj) =>
        val Some((_, ref)) = refTypeClass.findRef(obj)
        resolve(obj, Pointer(ref), updatedScope.addVisited(ref)).right.flatMap(r =>
          resolve(r.resolved, pointer, r.scope)
        )

      case (_, _) if pointer.hasProtocol && pointer.hasFragment =>
        // change document root
        for {
          url <- createUrl(pointer).right
          fetchedSchema <- fetch(url, scope).right
          res <- resolve(fetchedSchema, pointer.fragments.getOrElse(Pointers.`#`), updatedScope.copy(documentRoot = fetchedSchema)).right
        } yield res

      case (_, Pointers.`#`) =>
        Right(ResolvedResult(updatedScope.documentRoot, updatedScope))

      case (_, _) if pointer.isFragment =>
        resolve(updatedScope.documentRoot, pointer.dropHashAtBeginning, updatedScope)

      case (container, _) =>
        resolveRelative(pointer, updatedScope) orElse
          resolveFragments(toFragments(pointer), updatedScope, container) orElse
          resolveDefinition(pointer, updatedScope).left.map(_ =>
            ValidationError(s"Could not resolve ref $pointer")
          )
    }

    result match {
      case Right(ResolvedResult(r, s)) =>
        refTypeClass.findRef(r).fold(result) { case (_, refValue) => resolve(Pointer(refValue), s) }
      case other => other
    }
  }

  private def resolveDefinition(pointer: Pointer, scope: GenResolutionScope[A]): Either[ValidationError, ResolvedResult[A]] = pointer match {
    case Pointer(value) if value.contains("definitions") =>
      Left(ValidationError(s"Could not resolve ref $pointer"))
    case _ => resolve(scope.documentRoot, Pointer("#/definitions/" + pointer.value), scope)
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
      case _            => Left(ValidationError(s"Could not resolve ref $pointer"))
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
        for {
          resolved <- refTypeClass.resolve(resolvable, fragment).right
          result <- resolveFragments(rest,
            updateResolutionScope(scope, resolved).copy(
              schemaPath = scope.schemaPath.compose(JsPath \ fragment),
              instancePath = scope.instancePath.compose(JsPath \ fragment)
            ),
            resolved
          ).right
        } yield result
    }
  }

  /**
    * Fetch an instance from the given URL.
    *
    * @param url the absolute URL string from which to fetch schema
    * @return the resolved instance contained in a right-biased Either
    */
  private[schema] def fetch(url: URL, scope: GenResolutionScope[A]): Either[ValidationError, A] = {
    Try { Source.fromURL(url) }
      .toEither
      .right
      .flatMap(source =>
        pointerToSchemaCache.get(url.toString) match {
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
            resolved.right.map {
              pointerToSchemaCache.addId(normalize(Pointer(url.toString), scope))
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
    val normalized = normalize(ref.documentName, scope)

    val res = for {
      url           <- createUrl(normalized).right
      fetchedSchema <- fetch(url, scope).right
      result <- resolve(fetchedSchema,
        ref.fragments.getOrElse(Pointers.`#`),
        scope.copy(documentRoot = fetchedSchema)
      ).right
    } yield result
    res match {
      case Right(_) => res
      case Left(_) =>
        pointerToSchemaCache.getId(ref).fold(res) { found =>
          Right(ResolvedResult(found, scope))
        }
    }
  }

  /**
    * Split the given pointer into single fragments.
    * If the given URI is an absolute path the base URI
    * will be ignored.
    *
    * @param pointer the pointer
    * @return the single fragments as a list
    */
  private[schema] def toFragments(pointer: Pointer): List[String] = {

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
