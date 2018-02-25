package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaResolutionContext, SchemaString}
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

object StringValidator extends SchemaTypeValidator[SchemaString] {

  override def validate(schema: SchemaString, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    schema.constraints.validate(schema, json, context)
  }
}
