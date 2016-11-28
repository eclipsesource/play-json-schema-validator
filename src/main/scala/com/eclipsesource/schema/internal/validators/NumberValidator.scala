package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaNumber
import com.eclipsesource.schema.internal.SchemaRefResolver.SchemaResolutionContext
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json.JsValue

object NumberValidator extends SchemaTypeValidator[SchemaNumber] with NumberConstraintsValidator {
  override def validate(schema: SchemaNumber, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    val reader = for {
      maxRule <- validateMax
      minRule <- validateMin
      multipleOfRule <- validateMultipleOf
    } yield maxRule |+| minRule |+| multipleOfRule
    reader.run((schema.constraints, context))
      .repath(_.compose(context.instancePath))
      .validate(json)
  }
}
