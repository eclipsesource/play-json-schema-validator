package com.eclipsesource.schema.internal.draft7.constraints

import com.eclipsesource.schema.{SchemaInteger, SchemaNumber, SchemaResolutionContext, SchemaType, SchemaValue}
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil, ValidatorMessages}
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.osinka.i18n.Lang
import play.api.libs.json.{JsNumber, JsString, JsValue}
import scalaz.Success

case class NumberConstraints7(min: Option[Minimum] = None,
                             max: Option[Maximum] = None,
                             multipleOf: Option[BigDecimal] = None,
                             format: Option[String] = None,
                             any: AnyConstraints = AnyConstraints7()
                            ) extends HasAnyConstraint with NumberConstraints {

  import com.eclipsesource.schema.internal.validators.NumberValidators._

  override def subSchemas: Set[SchemaType] = any.subSchemas

  override def resolvePath(path: String): Option[SchemaType] = path match {
    case Keywords.Number.Min => min.map(m => SchemaValue(JsNumber(m.min)))
    case Keywords.Number.Max => max.map(m => SchemaValue(JsNumber(m.max)))
    case Keywords.Number.MultipleOf => multipleOf.map(m => SchemaValue(JsNumber(m)))
    case Keywords.String.Format => format.map(f => SchemaValue(JsString(f)))
    case other => any.resolvePath(other)
  }

  def isInt(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping {
        case json@JsNumber(number) if number.isWhole => Success(json)
        case other =>
          SchemaUtil.failure(
            Keywords.Any.Type,
            ValidatorMessages("err.expected.type", "integer", SchemaUtil.typeOfAsString(other)),
            context.schemaPath,
            context.instancePath,
            other
          )
      }
    }

  override def validate(schema: SchemaType, json: JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {

    val reader = for {
      maxRule <- validateMax(max)
      minRule <- validateMin(min)
      multipleOfRule <- validateMultipleOf(multipleOf)
      format <- validateFormat(format)
    } yield maxRule |+| minRule |+| multipleOfRule |+| format

    schema match {
      case SchemaInteger(_) =>
        isInt.flatMap(x => reader.map(y => x |+| y))
          .run(context)
          .repath(_.compose(context.instancePath))
          .validate(json)
      case SchemaNumber(_) =>
        reader
          .run(context)
          .repath(_.compose(context.instancePath))
          .validate(json)
      case _ => Success(json)
    }
  }
}