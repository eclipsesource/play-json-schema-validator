package com.eclipsesource

import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{Results, Context, SchemaUtil}
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import play.api.data.validation.ValidationError
import play.api.libs.json._
import scalaz.Success

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaReads {

  implicit def noValidator[S <: SchemaType] = new SchemaTypeValidator[S] {
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

  implicit class SchemaTypeExtensionOps[S <: SchemaType](schemaType: S) {

    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)

    def validate(json: => JsValue, context: Context)(implicit validator: SchemaTypeValidator[S]): VA[JsValue] = {
      Results.merge(
        validator.validate(schemaType, json, context),
        AnyConstraintValidator.validate(json, schemaType.constraints.any, context)
      )
    }
  }

  implicit class FailureExtensions(errors: Seq[(JsPath, Seq[ValidationError])]) {
    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}