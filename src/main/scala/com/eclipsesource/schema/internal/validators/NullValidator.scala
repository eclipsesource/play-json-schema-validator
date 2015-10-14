package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaNull
import com.eclipsesource.schema.internal.{SchemaUtil, Results, Context}
import play.api.data.mapping.{Success, VA}
import play.api.libs.json.{JsNull, JsValue}

object NullValidator extends SchemaTypeValidator[SchemaNull] {
  override def validate(schema: SchemaNull, json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsNull => Success(json)
    case _ => Results.failureWithPath(
      s"Wrong type. Expected null, got ${SchemaUtil.typeOfAsString(json)}",
      context.schemaPath.toString(),
      context.instancePath.toString(),
      context.root,
      json
    )
  }
}
