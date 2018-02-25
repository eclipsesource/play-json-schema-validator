package com.eclipsesource.schema

import java.net.{URL, URLStreamHandler}

import com.eclipsesource.schema.internal.constraints.Constraints.HasAnyConstraint
import com.eclipsesource.schema.internal.refs.{Ref, SchemaRefResolver, SchemaResolutionScope}
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.urlhandlers.UrlHandler
import com.osinka.i18n.Lang
import play.api.libs.json._

import scala.io.Source
import scalaz.{-\/, \/, \/-}

/**
  * Allows customizations of the validation process.
  */
trait Customizations {
  def refResolver: SchemaRefResolver
  def formats: Map[String, SchemaFormat]
}

object SchemaValidator {
  def apply(version: SchemaVersion, formats: Map[String, SchemaFormat] = DefaultFormats.formats)
           (implicit lang: Lang = Lang.Default): SchemaValidator = {
    new SchemaValidator(SchemaRefResolver(version), formats)
  }
}

/**
  * The schema validator.
  *
  */
class SchemaValidator(val refResolver: SchemaRefResolver,
                      val formats: Map[String, SchemaFormat])
                     (implicit val lang: Lang)
  extends Customizations {


  /**
    * Add a URLStreamHandler that is capable of handling absolute with a specific scheme.
    *
    * @param handler the UrlHandler to be added
    * @return a new validator instance
    */
  def addUrlHandler(handler: URLStreamHandler, scheme: String): SchemaValidator = {
    new SchemaValidator(
      refResolver.copy(resolverFactory = refResolver.resolverFactory.addUrlHandler(scheme, handler)),
      formats
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
      refResolver.copy(resolverFactory = refResolver.resolverFactory.addRelativeUrlHandler(scheme, handler)),
      formats
    )
  }


  /**
    * Add a custom format
    *
    * @param format the custom format
    * @return a new validator instance containing the custom format
    */
  def addFormat(format: SchemaFormat): SchemaValidator =
    new SchemaValidator(refResolver, formats + (format.name -> format))

  /**
    * Add a schema.
    *
    * @param id the id of the schema
    * @param schema the schema
    */
  def addSchema(id: String, schema: SchemaType): SchemaValidator = {
    new SchemaValidator(
      refResolver.copy(cache = refResolver.cache.add(Ref(id))(schema)),
      formats
    )
  }

  private def buildContext(schema: SchemaType, schemaUrl: URL): SchemaResolutionContext = schema.constraints match {
    case c: HasAnyConstraint =>
      val id = c.any.id.map(Ref(_))
      SchemaResolutionContext(refResolver,
        SchemaResolutionScope(schema, id.orElse(Some(Ref(schemaUrl.toString)))),
        formats = formats
      )
    case _ =>
      SchemaResolutionContext(refResolver,
        SchemaResolutionScope(schema, Some(Ref(schemaUrl.toString))),
        formats = formats
      )
  }

  /**
    * Validate the given JsValue against the schema located at the given URL.
    *
    * @param schemaUrl an URL pointing to a schema
    * @param input the value to be validated
    * @return a JsResult holding the validation result
    */
  def validate(schemaUrl: URL, input: => JsValue): JsResult[JsValue] = {

    def toJsResult(either: \/[JsonValidationError, SchemaType]): JsResult[SchemaType] = either match {
      case -\/(errs) => JsError(errs)
      case \/-(schema) => JsSuccess(schema)
    }

    val ref = Ref(schemaUrl.toString)
    refResolver.cache.get(ref.value) match {
      case None =>
        for {
          schema <- toJsResult(refResolver.readSource(Source.fromURL(schemaUrl)))
          context = buildContext(schema, schemaUrl)
          result <- schema.validate(input, context).toJsResult
        } yield {
          refResolver.cache = refResolver.cache.add(ref)(schema)
          result
        }
      case Some(schema) =>
        val context = buildContext(schema, schemaUrl)
        schema.validate(input, context).toJsResult
    }
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
    val ref: Option[Ref] = schema.constraints match {
      case c: HasAnyConstraint => c.any.id.map(Ref(_))
      case _ => None
    }

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
