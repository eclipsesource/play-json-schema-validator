package com.eclipsesource.schema.internal.refs

import java.net.{URL, URLDecoder, URLStreamHandler}

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.constraints.Constraints.Constraint
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import com.osinka.i18n.Lang
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
  cache: DocumentCache = DocumentCache(),
  resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory()
) {

  import version._

  val MaxDepth: Int = 100

  /**
    * Update the resolution scope based on the current element.
    *
    * @param scope the current resolution scope
    * @param schema the value that might contain scope refinements
    * @return the updated scope, if the given value contain a scope refinement, otherwise
    *         the not updated scope
    */
  private[schema] def updateResolutionScope(scope: SchemaResolutionScope, schema: SchemaType): SchemaResolutionScope = schema match {
    case _ if refinesScope(schema) =>
      val updatedId = findScopeRefinement(schema).map(
        id => Refs.mergeRefs(id, scope.id, Some(resolverFactory))
      )
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
      JsonValidationError(ValidatorMessages("err.max.depth")).left
    } else {
      val result: \/[JsonValidationError, ResolvedResult] = ref match {

        case l@LocalRef(_) =>
          resolveLocal(splitFragment(l), scope, current)

        case r if cache.contains(r) =>
          val resolvedSchema: SchemaType = cache(r)
          ResolvedResult(
            resolvedSchema,
            scope.copy(
              id = Some(Refs.mergeRefs(r, updatedScope.id)),
              documentRoot = resolvedSchema
            )
          ).right[JsonValidationError]

        // check if any prefix of ref matches current element
        case a@AbsoluteRef(absoluteRef)  =>
          val currentResolutionScope = findScopeRefinement(current)

          currentResolutionScope.collectFirst {
            case id if absoluteRef.startsWith(id.value) => absoluteRef.drop(id.value.length)
          }.map(remaining =>
            resolve(
              current,
              Ref(if (remaining.startsWith("#")) remaining else "#" + remaining),
              updatedScope
            )
          ).getOrElse(resolveAbsolute(a, updatedScope))

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
        case _ => resolutionFailure(ref).left
      }
    }
  }
  private[schema] def resolutionFailure(ref: Ref)(implicit lang: Lang): JsonValidationError =
    JsonValidationError(ValidatorMessages("err.unresolved.ref", ref.value))

  private def resolveRelative(ref: RelativeRef, scope: SchemaResolutionScope, instance: SchemaType)
                             (implicit lang: Lang): \/[JsonValidationError, ResolvedResult] = {
    Refs.mergeRefs(ref, scope.id) match {
      case a@AbsoluteRef(_) =>
        resolve(instance, a, scope)
      case r@RelativeRef(relativeRef) =>
        val (file, localRef) = relativeRef.splitAt(relativeRef.indexOf("#"))
        val result = for {
          schema <- cache.get(file)
        } yield {
          resolve(
            schema,
            LocalRef(localRef),
            scope.copy(
              documentRoot = schema,
              id = updateResolutionScope(scope, schema).id orElse Some(Ref(file)),
              referrer = scope.schemaJsPath
            )
          )
        }
        result.getOrElse(resolutionFailure(r).left)
    }
  }

  private[schema] def resolveLocal(schemaPath: List[String], scope: SchemaResolutionScope, instance: SchemaType)
                                  (implicit lang: Lang): \/[JsonValidationError, ResolvedResult] =
    (schemaPath, instance) match {
      case (Nil, _) =>
        \/.fromEither(
          resolveSchema(instance, "")
            .map(resolved => ResolvedResult(resolved, scope))
        ) orElse ResolvedResult(instance, scope).right
      case (_, SchemaRef(ref, _, _)) if !ref.isInstanceOf[LocalRef] =>
        resolve(scope.documentRoot, ref, scope).flatMap(resolvedResult =>
          resolveLocal(schemaPath, resolvedResult.scope, resolvedResult.resolved)
        )
      case (schemaProp :: rest, resolvable) =>
        schemaProp match {
          case "#" => resolveLocal(rest, scope.copy(schemaJsPath = Some(JsPath \ "#")), scope.documentRoot)
          case _ => \/.fromEither(resolveSchema(resolvable, schemaProp)).flatMap { r =>
            val newScope = updateResolutionScope(scope, r)
            resolveLocal(
              rest,
              newScope.copy(
                schemaJsPath =
                  if (resolutionScopeChanged(newScope.id, scope.id)) None
                  else scope.schemaJsPath.map(_.compose(JsPath \ schemaProp))
              ),
              r
            )
          }
        }
    }

  private def resolutionScopeChanged(oldScope: Option[Ref], newScope: Option[Ref]) = {
    val changed = for {
      n <- newScope
      o <- oldScope
    } yield n != o
    changed.getOrElse(false)
  }

  private def createUrl(ref: Ref)(implicit lang: Lang): \/[JsonValidationError, URL] = {
    // use handlers for protocol-ful absolute refs or fall back to default behaviour via null
    val handler: URLStreamHandler = ref.scheme.map(resolverFactory.createURLStreamHandler).orNull
    val triedUrl = Try { new URL(null, ref.value, handler) }
    triedUrl match {
      case Success(url) => url.right
      case _ => resolutionFailure(ref).left
    }
  }

  /**
    * Fetch the instance located at the given URL and eventually cache it as well.
    *
    * @param ref the ref to fetch from
    * @param scope the current resolution scope
    * @return the fetched instance, if any
    */
  private[schema] def fetch(ref: Ref, scope: SchemaResolutionScope)(implicit lang: Lang): \/[JsonValidationError, SchemaType] = {
    cache.get(ref.value) orElse cache.get(ref.value + "#") match {
      case Some(a) => a.right
      case _ => for {
        url <- createUrl(ref)
        source <- if (url.getProtocol == null || version.options.supportsExternalReferences) {
          \/.fromEither(Try { Source.fromURL(url) }.toJsonEither)
        } else {
          JsonValidationError(ValidatorMessages("err.unresolved.ref")).left
        }
        read <- readSource(source)
      } yield {
        cache.add(Refs.mergeRefs(ref, scope.id, Some(resolverFactory)))(read)
        read
      }
    }
  }

  private def parseJson(source: Source): \/[JsonValidationError, JsValue] = \/.fromEither(Try {
    Json.parse(source.getLines().mkString)
  }.toJsonEither)

  private[schema] def readJson(json: JsValue)(implicit lang: Lang): \/[JsonValidationError, SchemaType] = \/.fromEither(Json.fromJson[SchemaType](json).asEither)
    .leftMap(errors =>
      JsonValidationError(ValidatorMessages("err.parse.json"), JsError.toJson(errors))
    )

  private[schema] def readSource(source: Source)(implicit lang: Lang): \/[JsonValidationError, SchemaType] = {
    using(source) { src =>
      for {
        json <- parseJson(src)
        resolvedSchema <- readJson(json)
      } yield resolvedSchema
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
      fetchedSchema <- fetch(ref.documentName, scope)
      result <- resolve(fetchedSchema,
        ref.pointer.getOrElse(Refs.`#`),
        scope.copy(
          id = Some(ref.documentName),
          documentRoot = fetchedSchema,
          referrer = scope.schemaJsPath

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

    ref.pointer.map(_.value)
      .getOrElse(ref.value)
      .split("/").toList
      .map(escape)
  }

  def refinesScope(a: SchemaType): Boolean = findScopeRefinement(a).isDefined

  def findRef(schema: SchemaType): Option[Ref] = schema match {
    case SchemaRef(ref, _, _) => Some(ref)
    case _ => None
  }

  def findScopeRefinement(schema: SchemaType): Option[Ref] = schema.constraints.id.map(Ref(_))

  private def resolveConstraint[A <: Constraint](constraints: A, constraint: String)
                                                (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
    constraints.resolvePath(constraint).fold[Either[JsonValidationError, SchemaType]](
      Left(JsonValidationError(ValidatorMessages("err.unresolved.ref", constraint)))
    )(schema => Right(schema))
  }

  private def findProp(props: Seq[SchemaProp], propName: String)
                                 (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
    props.collectFirst {
      case SchemaProp(name, s) if name == propName => s
    }.toRight(JsonValidationError(ValidatorMessages("err.prop.not.found", propName)))
  }

  private def findOtherProp(props: Seq[(String, SchemaType)], propName: String)
                                 (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
    props.collectFirst {
      case (name, s) if name == propName => s
    }.toRight(JsonValidationError(ValidatorMessages("err.prop.not.found", propName)))
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
        members.find(_.name == fragmentPart).map(_.schemaType).toRight(JsonValidationError(ValidatorMessages(s"err.$name.not.found")))

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
            Left(JsonValidationError(ValidatorMessages("arr.out.of.bounds", index)))
          }
        case _ => Left(JsonValidationError(ValidatorMessages("arr.invalid.index", fragmentPart)))
      }

      case CompoundSchemaType(alternatives) =>
        val results = alternatives.map(
          alternative => resolveSchema(alternative, fragmentPart)
        )
        results
          .collectFirst { case r@Right(_) => r }
          .getOrElse(Left(JsonValidationError(ValidatorMessages("err.unresolved.ref", fragmentPart))))

      case n: SchemaNumber => resolveConstraint(n.constraints, fragmentPart)
      case n: SchemaInteger => resolveConstraint(n.constraints, fragmentPart)
      case n: SchemaBoolean => resolveConstraint(n.constraints, fragmentPart)
      case n: SchemaString =>
        resolveConstraint(n.constraints, fragmentPart)
      case r: SchemaRef =>
        findOtherProp(r.otherProps, fragmentPart) orElse resolveConstraint(r.constraints, fragmentPart)
      case SchemaRoot(_, s) => resolveSchema(s, fragmentPart)

    }
  }
}
