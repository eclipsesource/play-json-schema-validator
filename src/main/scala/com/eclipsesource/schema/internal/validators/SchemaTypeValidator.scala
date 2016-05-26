package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.ResolutionContext
import com.eclipsesource.schema.internal.validation.VA
import play.api.libs.json.JsValue

trait SchemaTypeValidator[S] {
  def validate(schema: S, json: => JsValue, resolutionContext: ResolutionContext): VA[JsValue]
}


