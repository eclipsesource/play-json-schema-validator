package com.eclipsesource.schema.internal.validators
import com.eclipsesource.schema._
import com.eclipsesource.schema.CompoundSchemaType
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.libs.json.{JsError, JsSuccess, JsValue}

import scalaz.{Failure, Success}

object CompoundValidator extends SchemaTypeValidator[CompoundSchemaType] {
  override def validate(schema: CompoundSchemaType, json: => JsValue, context: Context): VA[JsValue] = {
    val result: Option[VA[JsValue]] = schema.alternatives
      .map(s =>
        SchemaValidator.validate(s, json) match {
          case JsSuccess(success, _) => Success(success)
          case JsError(errors) => Failure(errors)
        }
      ).find(_.isSuccess)
    result
      .getOrElse(
        Results.failureWithPath(
          "No schema applied",
          context.schemaPath,
          context.instancePath,
          json
        )
      )
  }
}
