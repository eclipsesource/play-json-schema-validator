package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaNull
import com.eclipsesource.schema.internal.{Results, Context}
import play.api.data.mapping.{Success, VA}
import play.api.libs.json.{JsNull, JsValue}

object NullValidator extends Validator2[SchemaNull] {
  override def validate(schema: SchemaNull, json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsNull => Success(json)
    case _ => Results.failure("Expected null")
  }
}
