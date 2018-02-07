package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaResolutionContext
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

trait SchemaTypeValidator[S] {
  def validate(schema: S, json: => JsValue, resolutionContext: SchemaResolutionContext)
              (implicit lang: Lang = Lang.Default): VA[JsValue]
}


