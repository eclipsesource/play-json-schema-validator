package com.eclipsesource.schema

import java.net.{URL, URLStreamHandler}

import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema.internal.url.UrlResolver
import com.eclipsesource.schema.internal.validation.VA
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.Failure

trait HasRefResolver {
  def refResolver: SchemaRefResolver
}

trait CanValidate { self: HasRefResolver =>

  def validate(schemaUrl: URL, input: => JsValue): JsResult[JsValue] = {
    val schema = for {
      schemaJson <- JsonSource.fromURL(schemaUrl).toOption
      schema <- Json.fromJson[SchemaType](schemaJson).asOpt
    } yield schema

    val result: VA[JsValue] = schema match {
      case None => Failure(Seq(JsPath -> Seq(ValidationError("Schema can not be parsed."))))
      case Some(schemaType) =>
        val id = schemaType match {
          case container: HasId => container.id
          case _ => None
        }
        val context = new SchemaResolutionContext(refResolver, new SchemaResolutionScope(schemaType, id.orElse(Some(schemaUrl.toString)), Some(schemaUrl.toString)))
        schemaType.validate(input, context)
    }
    result.toJsResult
  }

  def validate[A](schemaUrl: URL, input: => JsValue, reads: Reads[A]) : JsResult[A] = {
    validate(schemaUrl, input).fold(
      valid = readAndValidate(reads),
      invalid = errors => JsError(essentialErrorInfo(errors, Some(input)))
    )
  }

  def validate[A](schemaUrl: URL, input: A, writes: Writes[A]): JsResult[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schemaUrl, inputJs)
  }

  def validate[A: Format](schemaUrl: URL, input: A): JsResult[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    validate(schemaUrl, input, writes).fold(
      valid = readAndValidate(reads),
      invalid = errors => JsError(essentialErrorInfo(errors, None))
    )
  }

  //
  // --
  //

  def validate(schema: SchemaType)(input: => JsValue): JsResult[JsValue] = {
    val id = schema match {
      case container: HasId => container.id
      case _ => None
    }
    val context = new SchemaResolutionContext(refResolver, new SchemaResolutionScope(schema, id, id))
    schema.validate(
      input,
      context
    ).toJsResult
  }

  def validate[A](schema: SchemaType, input: => JsValue, reads: Reads[A]) : JsResult[A] = {
    val result = validate(schema)(input)
    result.fold(
      valid = readAndValidate(reads),
      invalid  = errors => JsError(essentialErrorInfo(errors, Some(input)))
    )
  }

  def validate[A](schema: SchemaType, input: A, writes: Writes[A]): JsResult[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schema)(inputJs)
  }

  def validate[A: Format](schema: SchemaType, input: A): JsResult[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    val inputJs = writes.writes(input)
    val result = validate(schema)(inputJs)
    result.fold(
      valid = readAndValidate(reads),
      invalid = errors => JsError(essentialErrorInfo(errors, Some(inputJs)))
    )
  }

  private def readAndValidate[A](reads: Reads[A]): JsValue => JsResult[A] = json =>
    reads.reads(json) match {
      case JsSuccess(success, _) => JsSuccess(success)
      case JsError(errors) => JsError(essentialErrorInfo(errors, Some(json)))
    }

  private def essentialErrorInfo(errors: Seq[(JsPath, Seq[ValidationError])], json: Option[JsValue]): Seq[(JsPath, Seq[ValidationError])] = {

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
            case 0 => ValidationError(err.message,
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

case class SchemaValidator() extends CanValidate with HasRefResolver {
  override val refResolver: SchemaRefResolver = new SchemaRefResolver
  def addUrlHandler(protocolEntry: (String, URLStreamHandler)): SchemaValidator = {
    refResolver.addUrlHandler(protocolEntry)
    this
  }
  def addUrlResolver(urlResolver: UrlResolver): SchemaValidator = {
    refResolver.addUrlResolver(urlResolver)
    this
  }
}
