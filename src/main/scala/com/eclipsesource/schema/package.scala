package com.eclipsesource

import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{Results, Context, SchemaUtil}
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import play.api.data.mapping.{Success, VA}
import play.api.libs.json.JsValue

import scalaz.{Failure => _, Success => _}

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaReads {

  implicit def noValidator[S <: SchemaType] = new Validator2[S] {
        println("no validator")
        override def validate(schema: S, json: => JsValue, context: Context): VA[JsValue] = Success(json)
      }
  implicit val compoundValidator = CompoundValidator
  implicit val objectValidator = ObjectValidator
  implicit val arrayValidator = ArrayValidator
  implicit val tupleValidator = TupleValidator
  implicit val numberValidator = NumberValidator
  implicit val integerValidator = IntegerValidator
  implicit val stringValidator = StringValidator
  implicit val booleanValidator = noValidator[SchemaBoolean]
  implicit val nullValidator = noValidator[SchemaNull]
//  implicit val anyValidator = noValidator[SchemaType]

  implicit class SchemaTypeExtensionOps[S <: SchemaType](schemaType: S) {

    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)

    def validate(json: => JsValue, context: Context)(implicit validator: Validator2[S]): VA[JsValue] = {
      Results.merge(
        validator.validate(schemaType, json, context),
        AnyConstraintValidator.validate(json, schemaType.constraints.any, context)
      )
    }

  }

}