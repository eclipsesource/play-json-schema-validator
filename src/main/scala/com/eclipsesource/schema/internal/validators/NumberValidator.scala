package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.{SchemaNumber, SchemaResolutionContext}
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

object NumberValidator extends SchemaTypeValidator[SchemaNumber] {
  override def validate(schema: SchemaNumber, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    schema.constraints.validate(schema, json, context)
  }
}
