package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaNumber
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.SchemaRefResolver.SchemaResolutionContext
import com.eclipsesource.schema.internal.constraints.Constraints.NumberConstraints
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsNumber, JsValue}

import scalaz.Success

object NumberValidator extends SchemaTypeValidator[SchemaNumber] with NumberConstraintsValidator {
  override def validate(schema: SchemaNumber, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    val reader = for {
      maxRule <- validateMax
      minRule <- validateMin
      multipleOfRule <- validateMultipleOf
      format  <- validateFormat
    } yield maxRule |+| minRule |+| multipleOfRule |+| format
    reader.run((schema.constraints, context))
      .repath(_.compose(context.instancePath))
      .validate(json)
  }

  def validateFormat(implicit lang: Lang): scalaz.Reader[(NumberConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>

      val format = for {
        formatName <- constraints.format
        f <- context.formats.get(formatName)
      } yield f

      Rule.fromMapping {
        case json@JsNumber(number) if constraints.format.isDefined =>
          format match {
            // format found
            case Some(f) =>
              if (f.validate(json)) {
                Success(json)
              } else {
                failure(
                  Keywords.String.Format,
                  Messages("str.format", number, f.name),
                  context.schemaPath,
                  context.instancePath,
                  json
                )
              }
            // validation of unknown format should succeed
            case None => Success(json)
          }
        case json@JsNumber(_) => Success(json)
      }
    }
}
