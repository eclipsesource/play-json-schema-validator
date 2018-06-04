package com.eclipsesource.schema

import java.net.{URL, URLStreamHandler}

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.refs.{DocumentCache, Ref, SchemaRefResolver, SchemaResolutionScope}
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._
import scalaz.\/

import scala.io.Source
import scala.util.Try

/**
  * Allows customizations of the validation process.
  */
trait SchemaConfigOptions {
  def supportsExternalReferences: Boolean
  def formats: Map[String, SchemaFormat]
}

object SchemaValidator { self =>

  def apply(version: Option[SchemaVersion] = None)
           (implicit lang: Lang = Lang.Default): SchemaValidator = {
    val validator = new SchemaValidator(version, version.map(_.options.formats).getOrElse(DefaultFormats.formats))
    version.fold(validator) {
      case _: Version4 => validator.addSchema(Version4.SchemaUrl, Version4.Schema)
      case _: Version7 => validator.addSchema(Version7.SchemaUrl, Version7.Schema)
      case other =>
        throw new RuntimeException(s"Could not read schema file $other.")
    }

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
        .addAll(collectSchemas(schema, Some(Ref(id))))
    )
  }

  private def buildContext(refResolver: SchemaRefResolver, schema: SchemaType, schemaUrl: Option[Ref]): SchemaResolutionContext =
    SchemaResolutionContext(
      refResolver,
      SchemaResolutionScope(
        schema,
        schema.constraints.id.map(Ref(_)) orElse schemaUrl,
        Some(JsPath \ "#")
      ),
      formats = formats
    )

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

  def validate(schemaUrl: URL): JsValue => JsResult[JsValue] = {
    doValidate(Source.fromURL(schemaUrl), Some(Ref(schemaUrl.toString)))
  }

  def validate(schemaSource: Source): JsValue => JsResult[JsValue] = {
    doValidate(schemaSource, None)
  }

  private def doValidate(schemaSource: Source, schemaUrl: Option[Ref] = None): JsValue => JsResult[JsValue] = {
    val context: JsResult[SchemaResolutionContext] = parseJson(schemaSource).toJsResult.flatMap {
      json =>
        val version = obtainVersion(json)
        import version._
        val schema = readJson(json).toJsResult
        val id = schema.asOpt.flatMap(s => s.constraints.id.map(Ref(_))) orElse schemaUrl
        val refResolver = SchemaRefResolver(
          version,
          schema.fold(_ => cache, s => cache.addAll(collectSchemas(s, id))),
          resolverFactory
        )
        schema.map(s => buildContext(refResolver, s, id))
    }
    input: JsValue =>
      context
        .flatMap(ctx => ctx.scope.documentRoot.validate(input, ctx).toJsResult)
  }

  /**
    * Validate the given JsValue against the schema located at the given URL
    * and convert the result via the specified Reads instance in case
    * it has been successful.
    *
    * @param schemaSource source from where to read the schema from
    * @param input the value to be validated
    * @return a JsResult holding the validation result
    */
  def validate[A](schemaSource: Source, input: => JsValue, reads: Reads[A]): JsResult[A] = {
    validate(schemaSource)(input).fold(
      valid = readWith(reads),
      invalid = errors => JsError(errors)
    )
  }

  /**
    * Validate the given JsValue against the schema located at the given URL
    * and convert the result via the specified Reads instance in case
    * it has been successful.
    *
    * @param schemaUrl the URL from where to read the schema from
    * @param input the value to be validated
    * @return a JsResult holding the validation result
    */
  def validate[A](schemaUrl: URL, input: => JsValue, reads: Reads[A]): JsResult[A] = {
    validate(schemaUrl)(input).fold(
      valid = readWith(reads),
      invalid = errors => JsError(errors)
    )
  }

  /**
    * Convert the given value via the specified Writes instance to a JsValue
    * and validate it against the schema located at the given URL.
    *
    * @param schemaSource source from where to read the scheam from
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A](schemaSource: Source, input: => A, writes: Writes[A]): JsResult[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schemaSource)(inputJs)
  }

  /**
    * Convert the given value via the specified Writes instance to a JsValue
    * and validate it against the schema located at the given URL.
    *
    * @param schemaUrl the URL from where to read the schema from
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A](schemaUrl: URL, input: => A, writes: Writes[A]): JsResult[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schemaUrl)(inputJs)
  }

  /**
    * Convert the given value via the specified Format instance to a JsValue,
    * validate it against the schema at the given URL, and convert it back.
    *
    * @param schemaSource source from where to read the scheam from
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A : Format](schemaSource: Source, input: A): JsResult[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    validate(schemaSource, input, writes).fold(
      valid = readWith(reads),
      invalid = errors => JsError(errors)
    )
  }

  /**
    * Convert the given value via the specified Format instance to a JsValue,
    * validate it against the schema at the given URL, and convert it back.
    *
    * @param schemaUrl source from where to read the scheam from
    * @param input the value to be validated
    * @return a JsResult holding the valid result
    */
  def validate[A : Format](schemaUrl: URL, input: A): JsResult[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    validate(schemaUrl, input, writes).fold(
      valid = readWith(reads),
      invalid = errors => JsError(errors)
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
    val id = schema.constraints.id.map(Ref(_))
    val refResolver = SchemaRefResolver(version, cache.addAll(collectSchemas(schema, id)), resolverFactory)
    val context = SchemaResolutionContext(
      refResolver,
      SchemaResolutionScope(schema, ref, Some(JsPath \ "#")),
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
      invalid  = errors => JsError(errors)
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
    * validate it against the given schema and convert it back.
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
      invalid = errors => JsError(errors)
    )
  }

  private def readWith[A](reads: Reads[A]): JsValue => JsResult[A] = json =>
    reads.reads(json) match {
      case JsSuccess(success, _) => JsSuccess(success)
      case JsError(errors) => JsError(errors)
    }
}
