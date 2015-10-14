package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum, NumberConstraints}
import com.eclipsesource.schema.internal.{Context, Results, SchemaUtil}
import play.api.data.mapping.{Failure, Rule, Success}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsNumber, JsValue}

trait NumberConstraintsValidator {

  def validateMin: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] = {

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
              val minType = if (isExclusive) "exclusive minimum" else "minimum"
              val comparison = if (isExclusive) "less than or equal to" else "less than"
              Results.failure(
                s"$minType violated: $number is $comparison ${min.min}",
                context.schemaPath.toString(),
                context.instancePath.toString(),
                context.root,
                number
              )
            }
        }
        case other => expectedNumber(other, context)
      }
    }
  }

  def validateMax: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] = {

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
              val maxType = if (isExclusive) "exclusive maximum" else "maximum"
              val comparison = if (isExclusive) "bigger than or equal to" else "bigger than"
              Results.failure(
                s"$maxType violated: $number is $comparison ${max.max}",
                context.schemaPath.toString(),
                context.instancePath.toString(),
                context.root,
                number
              )
            }
        }
        case other => expectedNumber(other, context)
      }
    }
  }

  def validateMultipleOf: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraint, context) =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(n) => constraint.multipleOf match {
          case Some(factor) =>
            if (n.remainder(factor) == BigDecimal(0)) {
              Success(number)
            } else {
              Results.failure(
                s"$number is not a multiple of $factor.",
                context.schemaPath.toString(),
                context.instancePath.toString(),
                context.root,
                number
              )
            }
          case None => Success(number)
        }
        case other => expectedNumber(other, context)
      }
    }

  private def expectedNumber(json: JsValue, context: Context) =
    Results.failure(
      s"Wrong type. Expected number, was ${SchemaUtil.typeOfAsString(json)}",
      context.schemaPath.toString(),
      context.instancePath.toString(),
      context.root,
      json
    )
}
