package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaInteger, SchemaResolutionContext}
import com.eclipsesource.schema.internal.constraints.Constraints.NumberConstraints
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsNumber, JsValue}

import scalaz.Success

object IntegerValidator extends SchemaTypeValidator[SchemaInteger] with NumberConstraintsValidator {

  def isInt(implicit lang: Lang): scalaz.Reader[(NumberConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (_, context) =>
      Rule.fromMapping {
        case json@JsNumber(number) if number.isWhole()  => Success(json)
        case other =>
          failure(
            Keywords.Any.Type,
            Messages("err.expected.type", "integer", SchemaUtil.typeOfAsString(other)),
            context.schemaPath,
            context.instancePath,
            other
          )
      }
    }

  override def validate(schema: SchemaInteger, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    val reader = for {
      maxRule <- validateMax
      minRule <- validateMin
      multipleOfRule <- validateMultipleOf
      format <- validateFormat
      intRule <- isInt
    } yield maxRule |+| minRule |+| multipleOfRule |+| format |+| intRule
    reader.run((schema.constraints, context)).repath(_.compose(context.instancePath)).validate(json)
  }
}
