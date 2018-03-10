package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.{SchemaResolutionContext, SchemaType}
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

class DefaultValidator[A <: SchemaType] extends SchemaTypeValidator[A] {
  override def validate(schema: A, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    schema.constraints.validate(schema, json, context)
  }
}
