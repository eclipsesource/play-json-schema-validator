package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.{Context}
import play.api.data.mapping.VA
import play.api.libs.json.JsValue

trait SchemaTypeValidator[S] {
  def validate(schema: S, json: => JsValue, context: Context): VA[JsValue]
}


