package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.SchemaRefResolver.SchemaResolutionContext
import com.eclipsesource.schema.internal.validation.VA
import play.api.libs.json.JsValue

trait SchemaTypeValidator[S] {
  def validate(schema: S, json: => JsValue, resolutionContext: SchemaResolutionContext): VA[JsValue]
}


