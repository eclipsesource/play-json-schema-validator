package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaInteger
import com.eclipsesource.schema.internal.constraints.Constraints.NumberConstraints
import com.eclipsesource.schema.internal.{SchemaUtil, Results, Context}
import play.api.data.mapping.{Failure, Rule, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{Json, JsNumber, JsValue}

object IntegerValidator extends SchemaTypeValidator[SchemaInteger] with NumberConstraintsValidator {

  val isInt: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraint, context) =>
      Rule.fromMapping {
        case json@JsNumber(number) if number.isValidInt => Success(json)
        case other =>
          failure(
            s"Wrong type. Expected integer, was ${SchemaUtil.typeOfAsString(other)}",
            context.schemaPath.toString(),
            context.instancePath.toString(),
            other
          )
      }
    }

  override def validate(schema: SchemaInteger, json: => JsValue, context: Context): VA[JsValue] = {
    val reader = for {
      maxRule <- validateMax
      minRule <- validateMin
      multipleOfRule <- validateMultipleOf
      intRule <- isInt
    } yield maxRule |+| minRule |+| multipleOfRule |+| intRule
    reader.run((schema.constraints, context)).repath(_.compose(context.instancePath)).validate(json)
  }
}
