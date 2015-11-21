package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.internal._
import play.api.data.mapping.{Failure, Path, Success, VA}
import play.api.libs.json._

trait SchemaValidator {

  def validate(schemaUrl: URL, input: => JsValue): VA[JsValue] = {
    val schema = for {
      schemaJson <- JsonSource.fromURL(schemaUrl).toOption
      schema <- Json.fromJson[SchemaType](schemaJson).asOpt
    } yield schema

    schema match {
      case None => Failure(Seq())
      case Some(s) =>
        val id = s match {
          case container: HasId => container.id
          case _ => None
        }
        val path = schemaUrl.getFile.substring(0, schemaUrl.getFile.lastIndexOf('/'))
        val basePath = if (schemaUrl.getPort != -1) {
          new URL(schemaUrl.getProtocol + "://" + schemaUrl.getHost + ":" + schemaUrl.getPort + path)
        } else {
          new URL(schemaUrl.getProtocol + "://" + schemaUrl.getHost + path)
        }
        val context = Context(Path, Path, s, Set.empty, id, Some(basePath))
        val updatedRoot = RefResolver.replaceRefs(context)(s)
        process(
          updatedRoot,
          input,
          context.copy(root = updatedRoot)
        )
    }
  }

  def validate[A](schemaUrl: URL, input: => JsValue, reads: Reads[A]) : VA[A] = {
    validate(schemaUrl, input).fold(
      valid = {
        json => reads.reads(json) match {
          case JsSuccess(success, _) => Success(success)
          case JsError(errors) => Failure(errors.map(e => Path(e._1.toJsonString) -> e._2))
        }
      },
      invalid = errors => Failure(errors)
    )
  }

  def validate[A](schemaUrl: URL, input: A, writes: Writes[A]): VA[JsValue] = {
    val inputJs = writes.writes(input)
    validate(schemaUrl, inputJs)
  }

  def validate[A: Format](schemaUrl: URL, input: A): VA[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    GlobalContextCache.clear()
    validate(schemaUrl, input, writes).fold(
      valid = {
        json => reads.reads(json) match {
          case JsSuccess(success, _) => Success(success)
          case JsError(errors) => Failure(errors.map(e => Path(e._1.toJsonString) -> e._2))
        }
      },
      invalid = errors => Failure(errors)
    )
  }

  //
  // --
  //

  def validate(schema: SchemaType)(input: => JsValue): VA[JsValue] = {
    val id = schema match {
      case container: HasId => container.id
      case _ => None
    }
    val context = Context(Path, Path, schema, Set.empty, id)
    val updatedRoot = RefResolver.replaceRefs(context)(schema)
    GlobalContextCache.clear()
    process(
      updatedRoot,
      input,
      context.copy(root = updatedRoot)
    )
  }

  def validate[A](schema: SchemaType, input: => JsValue, reads: Reads[A]) : VA[A] = {
    val id = schema match {
      case container: HasId => container.id
      case _ => None
    }
    val context = Context(Path, Path, schema, Set.empty, id)
    val updatedRoot = RefResolver.replaceRefs(context)(schema)
    GlobalContextCache.clear()
    val result: VA[JsValue] = process(
      updatedRoot,
      input,
      context.copy(root = updatedRoot)
    )
    result.fold(
      valid = {
        json => reads.reads(json) match {
          case JsSuccess(success, _) => Success(success)
          case JsError(errors) => Failure(errors.map(e => Path(e._1.toJsonString) -> e._2))
        }
      },
      invalid = errors => Failure(errors)
    )
  }

  def validate[A](schema: SchemaType, input: A, writes: Writes[A]): VA[JsValue] = {
    val inputJs = writes.writes(input)
    val context = Context(Path, Path, schema, Set.empty)
    val updatedRoot = RefResolver.replaceRefs(context)(schema)
    GlobalContextCache.clear()
    process(updatedRoot, inputJs, context.copy(root = updatedRoot))
  }

  def validate[A: Format](schema: SchemaType, input: A): VA[A] = {
    val writes = implicitly[Writes[A]]
    val reads = implicitly[Reads[A]]
    val inputJs = writes.writes(input)
    val context = Context(Path, Path, schema, Set.empty)
    val updatedRoot = RefResolver.replaceRefs(context)(schema)
    GlobalContextCache.clear()
    val result: VA[JsValue] = process(
      updatedRoot,
      inputJs,
      context.copy(root = updatedRoot)
    )
    result.fold(
      valid = {
        json => reads.reads(json) match {
          case JsSuccess(success, _) => Success(success)
          case JsError(errors) => Failure(errors.map(e => Path(e._1.toJsonString) -> e._2))
        }
      },
      invalid = errors => Failure(errors)
    )
  }

  private[schema] def process(schema: SchemaType, json: JsValue, context: Context): VA[JsValue] = {

    (json, schema) match {
      case (_, schemaObject: SchemaObject) if schema.constraints.any.schemaTypeAsString.isEmpty =>
        schemaObject.validate(json, context)
      case (_: JsObject, schemaObject: SchemaObject) if schema.constraints.any.schemaTypeAsString.isDefined =>
        schemaObject.validate(json, context)
      case (_, c: CompoundSchemaType) =>
        c.validate(json, context)
      case (jsArray: JsArray, schemaArray: SchemaArray) =>
        schemaArray.validate(jsArray, context)
      case (jsArray: JsArray, schemaTuple: SchemaTuple) =>
        schemaTuple.validate(jsArray, context)
      case (jsNumber: JsNumber, schemaNumber: SchemaNumber) =>
        schemaNumber.validate(jsNumber, context)
      case (jsNumber: JsNumber, schemaInteger: SchemaInteger) =>
        schemaInteger.validate(jsNumber, context)
      case (jsBoolean: JsBoolean, schemaBoolean: SchemaBoolean) =>
        schemaBoolean.validate(jsBoolean, context)
      case (jsString: JsString, schemaString: SchemaString) =>
        schemaString.validate(jsString, context)
      case (JsNull, schemaNull: SchemaNull) =>
        schemaNull.validate(json, context)
      case (_, _) if schema.constraints.any.schemaTypeAsString.isEmpty =>
        Success(json)
      case _ =>
        Results.failureWithPath(s"Wrong type. Expected $schema, was ${SchemaUtil.typeOfAsString(json)}.",
          context.schemaPath,
          context.instancePath,
          json)
    }
  }
}

object SchemaValidator extends SchemaValidator
