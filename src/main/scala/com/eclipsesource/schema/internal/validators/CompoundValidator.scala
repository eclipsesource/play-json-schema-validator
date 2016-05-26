package com.eclipsesource.schema.internal.validators
import com.eclipsesource.schema._
import com.eclipsesource.schema.CompoundSchemaType
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.{ResolutionContext, Results}
import play.api.libs.json.JsValue

object CompoundValidator extends SchemaTypeValidator[CompoundSchemaType] {
  override def validate(schema: CompoundSchemaType, json: => JsValue, context: ResolutionContext): VA[JsValue] = {
    val result: Option[VA[JsValue]] = schema.alternatives
      .map(_.validate(json, context))
      .find(_.isSuccess)
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
