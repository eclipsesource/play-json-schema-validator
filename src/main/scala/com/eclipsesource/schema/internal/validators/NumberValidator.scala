package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaNumber, SchemaResolutionContext}
import com.eclipsesource.schema.internal.Keywords
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
}
