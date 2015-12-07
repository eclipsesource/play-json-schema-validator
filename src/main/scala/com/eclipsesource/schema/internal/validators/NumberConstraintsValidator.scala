package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.{Context, SchemaUtil}
import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum, NumberConstraints}
import play.api.data.mapping.{Rule, Success}
import play.api.libs.json.{JsNumber, JsValue}

trait NumberConstraintsValidator {

  val validateMin: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] = {

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
              failure(
                s"$minType violated: $number is $comparison ${min.min}",
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

  val validateMax: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] = {

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
              failure(
                s"$maxType violated: $number is $comparison ${max.max}",
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

  val validateMultipleOf: scalaz.Reader[(NumberConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      Rule.fromMapping[JsValue, JsValue] {
        case number@JsNumber(n) => constraints.multipleOf match {
          case Some(factor) =>
            if (n.remainder(factor) == BigDecimal(0)) {
              Success(number)
            } else {
              failure(
                s"$number is not a multiple of $factor.",
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

  private def expectedNumber(json: JsValue, context: Context) =
    failure(
      s"Wrong type. Expected number, was ${SchemaUtil.typeOfAsString(json)}",
      context.schemaPath,
      context.instancePath,
      json
    )
}
