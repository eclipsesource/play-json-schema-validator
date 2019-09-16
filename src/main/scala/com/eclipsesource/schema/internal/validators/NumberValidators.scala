package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaResolutionContext
import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum}
import com.eclipsesource.schema.internal.validation.Rule
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil, ValidatorMessages}
import com.osinka.i18n.Lang
import play.api.libs.json.{JsNumber, JsValue}
import scalaz.Success

object NumberValidators {

  def validateMin(minimum: Option[Minimum])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {

    def isValid(n: JsNumber, minConstraint: Minimum) = {
      if (minConstraint.isExclusive.getOrElse(false)) {
        n.value > minConstraint.min
      } else {
        n.value >= minConstraint.min
      }
    }

    scalaz.Reader { context =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(_) => minimum match {
          case None => Success(number)
          case Some(min) =>
            if (isValid(number, min)) {
              Success(number)
            } else {
              val isExclusive = min.isExclusive.getOrElse(false)
              val msg = if (isExclusive) {
                ValidatorMessages("num.min.exclusive", number, min.min)
              } else {
                ValidatorMessages("num.min", number, min.min)
              }
              SchemaUtil.failure(
                Keywords.Number.Min,
                msg,
                context.schemaPath,
                context.instancePath,
                number
              )
            }
        }
        case other => expectedNumber(other, context)
      }
    }
  }

  def validateMax(maximum: Option[Maximum])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {

    def isValid(n: JsNumber, maxConstraint: Maximum) = {
      if (maxConstraint.isExclusive.getOrElse(false)) {
        n.value < maxConstraint.max
      } else {
        n.value <= maxConstraint.max
      }
    }

    scalaz.Reader { context =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(_) => maximum match {
          case None => Success(number)
          case Some(max) =>
            if (isValid(number, max)) {
              Success(number)
            } else {
              val isExclusive = max.isExclusive.getOrElse(false)
              val msg = if (isExclusive) {
                ValidatorMessages("num.max.exclusive", number, max.max)
              } else {
                ValidatorMessages("num.max", number, max.max)
              }
              SchemaUtil.failure(
                Keywords.Number.Max,
                msg,
                context.schemaPath,
                context.instancePath,
                number
              )
            }
        }
        case other => expectedNumber(other, context)
      }
    }
  }

  def validateMultipleOf(multipleOf: Option[BigDecimal])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(n) => multipleOf match {
          case Some(factor) =>
            if (n.remainder(factor) == BigDecimal(0)) {
              Success(number)
            } else {
              SchemaUtil.failure(
                Keywords.Number.MultipleOf,
                ValidatorMessages("num.multiple.of", number, factor),
                context.schemaPath,
                context.instancePath,
                number
              )
            }
          case None => Success(number)
        }
        case other => expectedNumber(other, context)
      }
    }

  private def expectedNumber(json: JsValue, context: SchemaResolutionContext)
                            (implicit lang: Lang) =
    SchemaUtil.failure(
      Keywords.Any.Type,
      ValidatorMessages("err.expected.type", "number", SchemaUtil.typeOfAsString(json)),
      context.schemaPath,
      context.instancePath,
      json
    )


  def validateFormat(f: Option[String])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>

      val format = for {
        formatName <- f
        f <- context.formats.get(formatName)
      } yield f

      Rule.fromMapping {
        case json@JsNumber(number) if f.isDefined =>
          format match {
            // format found
            case Some(schemaFormat) =>
              if (schemaFormat.validate(json)) {
                Success(json)
              } else {
                SchemaUtil.failure(
                  Keywords.String.Format,
                  ValidatorMessages("str.format", number, schemaFormat.name),
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
