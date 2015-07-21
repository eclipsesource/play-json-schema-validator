package com.eclipsesource.schema

import com.eclipsesource.schema.internal.{Context, RefResolver}
import play.api.data.mapping.{Failure, Path, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json._

trait Validator {

  def validate(schema: SchemaType)(input: => JsValue): VA[JsValue] = {
    val id = schema match {
      case container: SchemaContainer => container.id
      case _ => None
    }
    val context = Context(Path, schema, Seq.empty, Set.empty, id)
    val updatedRoot = RefResolver.replaceRefs(context)(schema)
    process(
      updatedRoot,
      input,
      context.copy(root = updatedRoot)
    )
  }

  def validate[A: Writes](schema: SchemaType, input: A): VA[JsValue] = {
    val writer = implicitly[Writes[A]]
    val inputJs = writer.writes(input)
    val context = Context(Path, schema, Seq.empty, Set.empty)
    val updatedRoot = RefResolver.replaceRefs(context)(schema)
    process(updatedRoot, inputJs, context.copy(root = updatedRoot))
  }

  private[schema] def process(schema: SchemaType, json: JsValue, context: Context): VA[JsValue] = {

    (json, schema) match {
      case (_, schemaObject: SchemaObject) if !schema.constraints.any.schemaTypeAsString.isDefined =>
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
      case (undefined: JsUndefined, _) =>
        schema.validate(undefined, context)
      case (_, _) if !schema.constraints.any.schemaTypeAsString.isDefined =>
        Success(json)
      case _ =>
        Failure(List(context.path -> List(ValidationError("diff.types", Json.obj("schema" -> schema, "instance" -> json)))))
    }
  }
}

object Validator extends Validator
