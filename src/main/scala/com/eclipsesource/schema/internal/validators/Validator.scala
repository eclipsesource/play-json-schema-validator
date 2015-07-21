package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaObject, SchemaType}
import com.eclipsesource.schema.internal.constraints.Constraints.Constraint
import com.eclipsesource.schema.internal.{Results, Context}
import play.api.data.mapping.VA
import play.api.libs.json.JsValue

//trait CanBeValidated[S <: SchemaType] {
//  def validate(schema: S, json: => JsValue, context: Context)(implicit v: Validator[S]): VA[JsValue] = {
//    Results.merge(
//      v.validate(schema, json, context),
//      AnyConstraintValidator.validate(json, json, context)
//    )
//  }
//}

trait SchemaTypeValidator[S] {
  def validate(schema: S, json: => JsValue, context: Context): VA[JsValue]
}


