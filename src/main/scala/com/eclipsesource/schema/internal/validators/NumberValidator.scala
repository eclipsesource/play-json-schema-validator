package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaNumber
import com.eclipsesource.schema.internal.Context
import play.api.data.mapping.VA
import play.api.libs.json.JsValue

object NumberValidator extends SchemaTypeValidator[SchemaNumber] with NumberConstraintsValidator {
  override def validate(schema: SchemaNumber, json: => JsValue, context: Context): VA[JsValue] = {
    val constraint = schema.constraints
    (validateMax(constraint) |+|
      validateMin(constraint) |+|
      validateMultipleOf(constraint)
      ).validate(json)
  }
}
