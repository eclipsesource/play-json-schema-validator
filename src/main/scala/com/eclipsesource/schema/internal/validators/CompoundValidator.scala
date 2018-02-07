package com.eclipsesource.schema.internal.validators
import com.eclipsesource.schema.{CompoundSchemaType, _}
import com.eclipsesource.schema.internal.{Keywords, Results}
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.JsValue

object CompoundValidator extends SchemaTypeValidator[CompoundSchemaType] {
  override def validate(schema: CompoundSchemaType, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    val result: Option[VA[JsValue]] = schema.alternatives
      .map(_.validate(json, context))
      .find(_.isSuccess)

    result.getOrElse(
        Results.failureWithPath(
          Keywords.Any.Type,
          Messages("comp.no.schema"),
          context,
          json
        )
      )
  }
}
