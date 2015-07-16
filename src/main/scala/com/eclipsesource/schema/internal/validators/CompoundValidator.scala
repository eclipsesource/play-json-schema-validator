package com.eclipsesource.schema.internal.validators
import com.eclipsesource.schema._
import com.eclipsesource.schema.CompoundSchemaType
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.data.mapping.VA
import play.api.libs.json.JsValue

object CompoundValidator extends Validator2[CompoundSchemaType] {
  override def validate(schema: CompoundSchemaType, json: => JsValue, context: Context): VA[JsValue] = {
    val res = schema.oneOf.map(s => Validator.validate(s, json)).find(_.isSuccess).getOrElse(Results.failure("No schema applied"))
    println("compound "  +res)
    res
  }
}
