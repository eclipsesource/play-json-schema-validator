package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaInteger
import com.eclipsesource.schema.internal.{Results, Context}
import play.api.data.mapping.{Failure, Rule, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{Json, JsNumber, JsValue}

object IntegerValidator extends SchemaTypeValidator[SchemaInteger] with NumberConstraintsValidator {

  val isInt: Rule[JsValue, JsValue] = Rule.fromMapping {
    case json@JsNumber(number) if number.isValidInt => Success(json)
    case _ => Failure(
      Seq(
        ValidationError("Integer expected")
      )
    )
  }

  override def validate(schema: SchemaInteger, json: => JsValue, context: Context): VA[JsValue] = {
    val constraint = schema.constraints
    (validateMax(constraint) |+|
      validateMin(constraint) |+|
      validateMultipleOf(constraint) |+|
      isInt
      ).validate(json)
  }
}
