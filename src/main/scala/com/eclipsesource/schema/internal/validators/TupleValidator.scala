package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaResolutionContext, SchemaTuple}
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

object TupleValidator extends SchemaTypeValidator[SchemaTuple] {
  override def validate(schema: SchemaTuple, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    schema.constraints.validate(schema, json, context)
  }
}