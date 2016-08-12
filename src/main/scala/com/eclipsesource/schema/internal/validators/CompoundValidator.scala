package com.eclipsesource.schema.internal.validators
import com.eclipsesource.schema.{CompoundSchemaType, _}
import com.eclipsesource.schema.internal.Results
import com.eclipsesource.schema.internal.SchemaRefResolver.SchemaResolutionContext
import com.eclipsesource.schema.internal.validation.VA
import play.api.libs.json.JsValue

object CompoundValidator extends SchemaTypeValidator[CompoundSchemaType] {
  override def validate(schema: CompoundSchemaType, json: => JsValue, context: SchemaResolutionContext): VA[JsValue] = {
    val result: Option[VA[JsValue]] = schema.alternatives
      .map(_.validate(json, context))
      .find(_.isSuccess)

    result.getOrElse(
        Results.failureWithPath(
          "No schema applied",
          context,
          json
        )
      )
  }
}
