package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder, URLStreamHandler}

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.constraints.Constraints.{Constraint, HasAnyConstraint}
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import com.eclipsesource.schema.{CompoundSchemaType, SchemaArray, SchemaBoolean, SchemaInteger, SchemaMap, SchemaNumber, SchemaObject, SchemaProp, SchemaSeq, SchemaString, SchemaTuple, SchemaType, SchemaValue, SchemaVersion}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._
import scalaz.syntax.either._
import scalaz.{\/, \/-}

import scala.io.Source
import scala.util.{Success, Try}

case class ResolvedResult(resolved: SchemaType, scope: SchemaResolutionScope)

/**
  * Schema reference resolver.
  *
  */
case class SchemaRefResolver
(
  version: SchemaVersion,
  // TODO: try to avoid vars here
  resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory(),
  private[schema] var cache: DocumentCache = DocumentCache()
) {

  import version._

  val MaxDepth: Int = 100

  /**
    * Update the resolution scope based on the current element.
    *
    * @param scope the current resolution scope
    * @param a the value that might contain scope refinements
    * @return the updated scope, if the given value contain a scope refinement, otherwise
    *         the not updated scope
    */
  private[schema] def updateResolutionScope(scope: SchemaResolutionScope, a: SchemaType): SchemaResolutionScope = a match {
    case _ if refinesScope(a) =>
      val updatedId = findScopeRefinement(a).map(
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
  private[schema] def resolve(current: SchemaType, ref: Ref, scope: SchemaResolutionScope)
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult] = {

    // update resolution scope, if applicable
    val updatedScope = updateResolutionScope(scope.copy(depth = scope.depth + 1), current)

    if (scope.depth >= MaxDepth) {
      JsonValidationError(Messages("err.max.depth")).left
    } else {
      val result: \/[JsonValidationError, ResolvedResult] = ref match {

        case l@LocalRef(_) =>
          resolveLocal(splitFragment(l), scope, current)

          // check if ref is contained in knownSchemas
          // can also apply to relative URLs if not resolution scope is available
        case r if scope.knownSchemas.keySet.contains(r) =>
          ResolvedResult(scope.knownSchemas(r), scope).right[JsonValidationError]

        // check if any prefix of ref matches current element
        case a@AbsoluteRef(absoluteRef)  =>
          val currentResolutionScope = findScopeRefinement(current)

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
          findRef(resolved)
            .fold(result)(foundRef =>
              resolve(resolvedResult.resolved, foundRef, resolvedResult.scope)
            )
        case _ => resolutionFailure(ref)(updatedScope).left
      }
    }
  }

  private[schema] def resolutionFailure(ref: Ref)(scope: SchemaResolutionScope)
                               (implicit lang: Lang): JsonValidationError =
    JsonValidationError(Messages("err.unresolved.ref", ref.value))

  private def resolveRelative(ref: RelativeRef, scope: SchemaResolutionScope, instance: SchemaType)
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult] = {
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

  private[schema] def resolveLocal(schemaPath: List[String], scope: SchemaResolutionScope, instance: SchemaType)
                                  (implicit lang: Lang): \/[JsonValidationError, ResolvedResult] = {

    (schemaPath, instance) match {
      case (Nil, _) =>
        \/.fromEither(
          resolveSchema(instance, "")
            .map(resolved => ResolvedResult(resolved, scope))
        ) orElse ResolvedResult(instance, scope).right
      case (schemaProp :: rest, resolvable) =>
        schemaProp match {
          case "#" => resolveLocal(rest, scope.copy(schemaPath = JsPath \ "#"), scope.documentRoot)
          case _ => \/.fromEither(resolveSchema(resolvable, schemaProp)).flatMap { r =>
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
  private[schema] def fetch(url: URL, scope: SchemaResolutionScope)(implicit lang: Lang): \/[JsonValidationError, SchemaType] = {
    val ref = Ref(url.toString)
    cache.get(ref.value) match {
      case Some(a) => a.right
      case _ => for {
        source <- \/.fromEither(Try { Source.fromURL(url) }.toJsonEither)
        read <- readSource(source)
      } yield {
        cache = cache.add(Refs.mergeRefs(ref, scope.id, Some(resolverFactory)))(read)
        read
      }
    }
  }

  private def parseJson(source: Source): \/[JsonValidationError, JsValue] = \/.fromEither(Try {
    Json.parse(source.getLines().mkString)
  }.toJsonEither)

  private def readJson(json: JsValue)(implicit lang: Lang): \/[JsonValidationError, SchemaType] = \/.fromEither(Json.fromJson[SchemaType](json).asEither)
    .leftMap(errors =>
      JsonValidationError(Messages("err.parse.json"), JsError.toJson(errors))
    )

  private[schema] def readSource(source: Source)(implicit lang: Lang): \/[JsonValidationError, SchemaType] = {
    using(source) { src =>
      val resolved = for {
        json <- parseJson(src)
        resolvedSchema <- readJson(json)
      } yield resolvedSchema
      resolved
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
  private def resolveAbsolute(ref: AbsoluteRef, scope: SchemaResolutionScope)
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult] = {

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

  def refinesScope(a: SchemaType): Boolean = findScopeRefinement(a).isDefined


  def findRef(schema: SchemaType): Option[Ref] = schema match {
    case SchemaObject(props, _, _) =>
      props.collectFirst {
        case SchemaProp("$ref", SchemaValue(JsString(ref))) => Ref(ref)
      }
    case _ => None
  }

  def findScopeRefinement(schema: SchemaType): Option[Ref] = schema.constraints.id.map(Ref(_))

  private def resolveConstraint[A <: Constraint](constraints: A, constraint: String)
                                                // TODO: is the version still necessary with a typeclass?
                                                (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
    constraints.resolvePath(constraint).fold[Either[JsonValidationError, SchemaType]](
      Left(JsonValidationError(Messages("err.unresolved.ref", constraint)))
    )(schema => Right(schema))
  }

  private def findProp(props: Seq[SchemaProp], propName: String)
                                 (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
    props.collectFirst {
      case SchemaProp(name, s) if name == propName => s
    }.toRight(JsonValidationError(Messages("err.prop.not.found", propName)))
  }

  private def findOtherProp(props: Seq[(String, SchemaType)], propName: String)
                                 (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
    props.collectFirst {
      case (name, s) if name == propName => s
    }.toRight(JsonValidationError(Messages("err.prop.not.found", propName)))
  }


  def resolveSchema[A <: SchemaType](schema: A, fragmentPart: String)
                              (implicit lang: Lang = Lang.Default): Either[JsonValidationError, SchemaType] = {
    def isValidIndex(size: Int, idx: String) = {
      Try {
        val n = idx.toInt
        n <= size && n >= 0
      }.toOption.getOrElse(false)
    }

    schema match {

      case SchemaMap(name, members) =>
        members.find(_.name == fragmentPart).map(_.schemaType).toRight(JsonValidationError(Messages(s"err.$name.not.found")))

      case SchemaSeq(members) =>
        fragmentPart match {
          case idx if isValidIndex(members.size, idx) => Right(members(idx.toInt))
        }

      case obj@SchemaObject(props, _, otherProps) => fragmentPart match {
        case Keywords.Object.Properties => Right(obj)
        case _ =>
          resolveConstraint(obj.constraints, fragmentPart) orElse
            findProp(props, fragmentPart) orElse
            findOtherProp(otherProps, fragmentPart)
      }

      case arr@SchemaArray(items, _, otherProps) => fragmentPart match {
        case Keywords.Array.Items => Right(items)
        case other =>
          findOtherProp(otherProps, other)
            .map(Right(_))
            .getOrElse(resolveConstraint(arr.constraints, fragmentPart))
      }

      case tuple@SchemaTuple(items, _, _) =>
        fragmentPart match {
          case Keywords.Array.Items => Right(tuple)
          case idx if isValidIndex(items.size, idx) => Right(items(idx.toInt))
          case _ => resolveConstraint(tuple.constraints, fragmentPart)
        }

      case SchemaValue(value) => (value, fragmentPart) match {
        case (arr: JsArray, index) if Try {
          index.toInt
        }.isSuccess =>
          val idx = index.toInt
          if (idx > 0 && idx < arr.value.size) {
            Right(SchemaValue(arr.value(idx)))
          } else {
            Left(JsonValidationError(Messages("arr.out.of.bounds", index)))
          }
        case _ => Left(JsonValidationError(Messages("arr.invalid.index", fragmentPart)))
      }

      case CompoundSchemaType(alternatives) =>
        val results = alternatives.map(
          alternative => resolveSchema(alternative, fragmentPart)
        )
        results
          .collectFirst { case r@Right(_) => r }
          .getOrElse(Left(JsonValidationError(Messages("err.unresolved.ref", fragmentPart))))

      case n: SchemaNumber => resolveConstraint(n.constraints, fragmentPart)
      case n: SchemaInteger => resolveConstraint(n.constraints, fragmentPart)
      case n: SchemaBoolean => resolveConstraint(n.constraints, fragmentPart)
      case n: SchemaString => resolveConstraint(n.constraints, fragmentPart)
    }
  }

}
