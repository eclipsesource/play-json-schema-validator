package com.eclipsesource.schema

import java.net.{URL, URLStreamHandler}

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.internal.refs.{DocumentCache, Ref, SchemaRefResolver, SchemaResolutionScope}
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.urlhandlers.UrlHandler
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._
import scalaz.\/

import scala.io.Source
import scala.util.Try

/**
  * Allows customizations of the validation process.
  */
trait SchemaConfigOptions {
  def supportsCanonicalReferencing: Boolean
  def formats: Map[String, SchemaFormat]
}

object SchemaValidator {
  def apply(version: Option[SchemaVersion] = None)
           (implicit lang: Lang = Lang.Default): SchemaValidator = {
    new SchemaValidator(version, version.map(_.options.formats).getOrElse(DefaultFormats.formats))
  }
}

/**
  * The schema validator.
  *
  */
class SchemaValidator(
                       val schemaVersion: Option[SchemaVersion] = None,
                       val formats: Map[String, SchemaFormat] = DefaultFormats.formats,
                       val resolverFactory: UrlStreamResolverFactory = UrlStreamResolverFactory(),
                       val cache: DocumentCache = DocumentCache()
                     )(implicit val lang: Lang) {

  val DefaultVersion: SchemaVersion = Version7

  /**
    * Add a URLStreamHandler that is capable of handling absolute with a specific scheme.
    *
    * @param handler the UrlHandler to be added
    * @return a new validator instance
    */
  def addUrlHandler(handler: URLStreamHandler, scheme: String): SchemaValidator = {
    new SchemaValidator(
      schemaVersion,
      formats,
      resolverFactory.addUrlHandler(scheme, handler),
      cache
    )
  }

  /**
    * Add a relative URLStreamHandler that is capable of resolving relative references.
    * Optionally takes a protocol that determines for which schemes the
    * handler should be triggered.
    *
    * @param handler the relative handler to be added
    * @return the validator instance with the handler being added
    */
  def addRelativeUrlHandler(handler: URLStreamHandler, scheme: String = UrlHandler.ProtocolLessScheme): SchemaValidator = {
    new SchemaValidator(
      schemaVersion,
      formats,
      resolverFactory.addRelativeUrlHandler(scheme, handler),
      cache
    )
  }


  /**
    * Add a custom format
    *
    * @param format the custom format
    * @return a new validator instance containing the custom format
    */
  def addFormat(format: SchemaFormat): SchemaValidator =
    new SchemaValidator(schemaVersion, formats + (format.name -> format), resolverFactory, cache)

  /**
    * Add a schema.
    *
    * @param id the id of the schema
    * @param schema the schema
    */
  def addSchema(id: String, schema: SchemaType): SchemaValidator = {
    new SchemaValidator(
      schemaVersion,
      formats,
      resolverFactory,
      cache.add(Ref(id))(schema)
    )
  }

  private def buildContext(refResolver: SchemaRefResolver, schema: SchemaType, schemaUrl: URL): SchemaResolutionContext =
    SchemaResolutionContext(
      refResolver,
      SchemaResolutionScope(schema, schema.constraints.id.map(Ref(_)) orElse Some(Ref(schemaUrl.toString))),
      formats = formats
    )

  /**
    * Validate the given JsValue against the schema located at the given URL.
    *
    * @param schemaUrl an URL pointing to a schema
    * @param input the value to be validated
    * @return a JsResult holding the validation result
    */
  def validate(schemaUrl: URL, input: => JsValue): JsResult[JsValue] = {
    val ref = Ref(schemaUrl.toString)
    parseJson(Source.fromURL(schemaUrl)).toJsResult.flatMap {
      json =>
        val version = obtainVersion(json)
        import version._
        val refResolver = SchemaRefResolver(version, resolverFactory, cache)
        val schema = cache.get(ref.value).fold(readJson(json).toJsResult)(JsSuccess(_))
        for {
          s <- schema
          context = buildContext(refResolver, s, schemaUrl)
          result <- s.validate(input, context).toJsResult
        } yield result
    }
  }

  private[schema] def readJson(json: JsValue)(implicit reads: Reads[SchemaType], lang: Lang): \/[JsonValidationError, SchemaType] = {
    \/.fromEither(Json.fromJson[SchemaType](json).asEither)
      .leftMap(errors =>
        JsonValidationError(Messages("err.parse.json"), JsError.toJson(errors))
      )
  }

  private def parseJson(source: Source): \/[JsonValidationError, JsValue] = \/.fromEither(Try {
    Json.parse(source.getLines().mkString)
  }.toJsonEither)


  private def obtainVersion(json: JsValue): SchemaVersion = {
    val version = schemaVersion orElse (json \ "$schema").toOption.map {
      case JsString(Version4.SchemaUrl) => Version4
      case _ => DefaultVersion
    }
    version.getOrElse(DefaultVersion)
  }

  private def obtainVersion(schema: SchemaType): SchemaVersion = {
    val $schema = schema match {
      case SchemaRoot(v, _) => v
      case _ => None
    }
    val version = schemaVersion orElse $schema
    version.getOrElse(DefaultVersion)
  }

  /**
    * Validate the given JsValue against the schema located at the given URL
    * and convert the result via the specified Reads instance in case
    * it has been successful.
    *
    * @param schemaUrl an URL pointing to a schema
    * @param input the value to be validated
    * @return a JsResult holding the validation result
    */
  def validate[A](schemaUrl: URL, input: => JsValue, reads: Reads[A]): JsResult[A] = {
    validate(schemaUrl, input).fold(
      valid = readWith(reads),
      invalid = errors => JsError(essentialErrorInfo(errors, Some(input)))
    )
  }

  /**
    * Convert the given value via the specified Writes instance to a JsValue
    * and validate it against the schema located at the given URL.
    *
    * @param schemaUrl an URL pointing to a schema
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A](schemaUrl: URL, input: A, writes: Writes[A]): JsResult[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schemaUrl, inputJs)
  }

  /**
    * Convert the given value via the specified Format instance to a JsValue,
    * validate it against the schema at the given URL, and convert it back.
    *
    * @param schemaUrl an URL pointing to a schema
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A : Format](schemaUrl: URL, input: A): JsResult[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    validate(schemaUrl, input, writes).fold(
      valid = readWith(reads),
      invalid = errors => JsError(essentialErrorInfo(errors, None))
    )
  }

  //
  // --
  //

  /**
    * Validate the given JsValue against the given schema.
    *
    * @param schema the schema to validate against
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate(schema: SchemaType)(input: => JsValue): JsResult[JsValue] = {
    val ref: Option[Ref] = schema.constraints.id.map(Ref(_))
    val version: SchemaVersion = obtainVersion(schema)
    val refResolver = SchemaRefResolver(version, resolverFactory, cache)
    val context = SchemaResolutionContext(
      refResolver,
      SchemaResolutionScope(schema, ref),
      formats = formats
    )

    schema.validate(input, context).toJsResult
  }

  /**
    * Validate the given JsValue against the schema and convert the result
    * via the specified Reads instance in case it has been successful.
    *
    * @param schema the schema to validate against
    * @param input the value to be validated
    * @return a JsResult holding the validation result
    */
  def validate[A](schema: SchemaType, input: => JsValue, reads: Reads[A]): JsResult[A] = {
    val result = validate(schema)(input)
    result.fold(
      valid = readWith(reads),
      invalid  = errors => JsError(essentialErrorInfo(errors, Some(input)))
    )
  }

  /**
    * Convert the given value via the specified Writes instance to a JsValue
    * and validate it against the schema.
    *
    * @param schema the schema to validate against
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A](schema: SchemaType, input: A, writes: Writes[A]): JsResult[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schema)(inputJs)
  }

  /**
    * Convert the given value via the specified Format instance to a JsValue,
    * validate it against the schema at the given URL, and convert it back.
    *
    * @param schema the schema to validate against
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A: Format](schema: SchemaType, input: A): JsResult[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    val inputJs = writes.writes(input)
    val result = validate(schema)(inputJs)
    result.fold(
      valid = readWith(reads),
      invalid = errors => JsError(essentialErrorInfo(errors, Some(inputJs)))
    )
  }

  private def readWith[A](reads: Reads[A]): JsValue => JsResult[A] = json =>
    reads.reads(json) match {
      case JsSuccess(success, _) => JsSuccess(success)
      case JsError(errors) => JsError(essentialErrorInfo(errors, Some(json)))
    }

  private def essentialErrorInfo(errors: Seq[(JsPath, Seq[JsonValidationError])], json: Option[JsValue]): Seq[(JsPath, Seq[JsonValidationError])] = {

    def dropObjPrefix(path: String): String = {
      if (path.startsWith("/obj")) {
        "/" + path.substring(5)
      } else {
        path
      }
    }

    errors.map { case (path, validationErrors) =>
      path ->
        validationErrors.map(err =>
          err.args.size match {
            case 0 => JsonValidationError(err.message,
              Json.obj(
                "schemaPath" -> "n/a",
                "instancePath" -> dropObjPrefix(path.toString()),
                "value" -> json.fold[JsValue](Json.obj())(identity),
                "errors" -> Json.obj()
              )
            )
            case _ => err
          }
        )
    }
  }
}
