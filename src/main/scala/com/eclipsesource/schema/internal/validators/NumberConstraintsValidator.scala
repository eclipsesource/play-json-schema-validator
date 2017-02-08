package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.SchemaRefResolver.SchemaResolutionContext
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil}
import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum, NumberConstraints}
import com.eclipsesource.schema.internal.validation.Rule
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsNumber, JsValue}

import scalaz.Success

trait NumberConstraintsValidator {

  def validateMin(implicit lang: Lang): scalaz.Reader[(NumberConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] = {

    def isValid(n: JsNumber, minConstraint: Minimum) = {
      if (minConstraint.isExclusive.getOrElse(false)) {
        n.value > minConstraint.min
      } else {
        n.value >= minConstraint.min
      }
    }

    scalaz.Reader { case (constraint, context) =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(_) => constraint.min match {
          case None => Success(number)
          case Some(min) =>
            if (isValid(number, min)) {
              Success(number)
            } else {
              val isExclusive = min.isExclusive.getOrElse(false)
              val msg = if (isExclusive) {
                Messages("num.min.exclusive", number, min.min)
              } else {
                Messages("num.min", number, min.min)
              }
              failure(
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

  def validateMax(implicit lang: Lang): scalaz.Reader[(NumberConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] = {

    def isValid(n: JsNumber, maxConstraint: Maximum) = {
      if (maxConstraint.isExclusive.getOrElse(false)) {
        n.value < maxConstraint.max
      } else {
        n.value <= maxConstraint.max
      }
    }

    scalaz.Reader { case (constraint, context) =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(_) => constraint.max match {
          case None => Success(number)
          case Some(max) =>
            if (isValid(number, max)) {
              Success(number)
            } else {
              val isExclusive = max.isExclusive.getOrElse(false)
              val msg = if (isExclusive) {
                Messages("num.max.exclusive", number, max.max)
              } else {
                Messages("num.max", number, max.max)
              }
              failure(
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

  def validateMultipleOf(implicit lang: Lang): scalaz.Reader[(NumberConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(n) => constraints.multipleOf match {
          case Some(factor) =>
            if (n.remainder(factor) == BigDecimal(0)) {
              Success(number)
            } else {
              failure(
                Keywords.Number.MultipleOf,
                Messages("num.multiple.of", number, factor),
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
    failure(
      Keywords.Any.Type,
      Messages("err.expected.type", "number", SchemaUtil.typeOfAsString(json)),
      context.schemaPath,
      context.instancePath,
      json
    )


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
