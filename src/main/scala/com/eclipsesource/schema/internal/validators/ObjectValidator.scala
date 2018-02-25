package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json._

object ObjectValidator extends SchemaTypeValidator[SchemaObject] {

  override def validate(schema: SchemaObject, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    schema.constraints.validate(schema, json, context)
  }

}

