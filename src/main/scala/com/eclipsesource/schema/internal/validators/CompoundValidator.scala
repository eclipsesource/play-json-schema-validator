package com.eclipsesource.schema.internal.validators
import com.eclipsesource.schema._
import com.eclipsesource.schema.CompoundSchemaType
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.data.mapping.VA
import play.api.libs.json.JsValue

object CompoundValidator extends SchemaTypeValidator[CompoundSchemaType] {
  override def validate(schema: CompoundSchemaType, json: => JsValue, context: Context): VA[JsValue] = {
    schema.alternatives
      .map(s => SchemaValidator.validate(s, json))
      .find(_.isSuccess)
      .getOrElse(
        Results.error(
          "No schema applied",
          context.schemaPath.toString(),
          context.instancePath.toString(),
          context.root,
          json
        )
      )
  }
}
