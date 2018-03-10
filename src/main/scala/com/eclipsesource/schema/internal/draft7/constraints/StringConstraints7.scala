package com.eclipsesource.schema.internal.draft7.constraints

import com.eclipsesource.schema.{SchemaResolutionContext, SchemaType, SchemaValue}
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints.{AnyConstraints, HasAnyConstraint, StringConstraints}
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json.{JsNumber, JsString, JsValue}

case class StringConstraints7(minLength: Option[Int] = None,
                             maxLength: Option[Int] = None,
                             pattern: Option[String] = None,
                             format: Option[String] = None,
                             any: AnyConstraints = AnyConstraints7()
                            ) extends HasAnyConstraint with StringConstraints {

  import com.eclipsesource.schema.internal.validators.StringValidators._

  override def subSchemas: Set[SchemaType] = any.subSchemas

  override def resolvePath(path: String): Option[SchemaType] = path match {
    case Keywords.String.MinLength => minLength.map(min => SchemaValue(JsNumber(min)))
    case Keywords.String.MaxLength => maxLength.map(max => SchemaValue(JsNumber(max)))
    case Keywords.String.Pattern => pattern.map(p => SchemaValue(JsString(p)))
    case Keywords.String.Format => format.map(f => SchemaValue(JsString(f)))
    case other => any.resolvePath(other)
  }

  def validate(schema: SchemaType, json: JsValue, context: SchemaResolutionContext)
              (implicit lang: Lang): VA[JsValue] = {
    val reader = for {
      minLength <- validateMinLength(minLength)
      maxLength <- validateMaxLength(maxLength)
      pattern <- validatePattern(pattern)
      format <- validateFormat(format)
    } yield minLength |+| maxLength |+| pattern |+| format
    reader.run(context)
      .repath(_.compose(context.instancePath))
      .validate(json)
  }
}