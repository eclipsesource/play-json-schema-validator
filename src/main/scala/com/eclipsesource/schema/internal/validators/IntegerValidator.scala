package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.{SchemaInteger, SchemaResolutionContext}
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

object IntegerValidator extends SchemaTypeValidator[SchemaInteger] {
  override def validate(schema: SchemaInteger, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    schema.constraints.validate(schema, json, context)
  }
}
