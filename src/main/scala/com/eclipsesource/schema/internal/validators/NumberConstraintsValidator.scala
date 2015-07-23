package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum, NumberConstraints}
import play.api.data.mapping.{VA, Rule, Success, Failure}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsValue, JsNumber, Json}

trait NumberConstraintsValidator {

//  def validateConstraints(json: JsValue, constraint: NumberConstraints): VA[JsValue] = {
//    (
//      validateMax(constraint) |+| validateMin(constraint) |+| validateMultipleOf(constraint)
//      ).validate(json)
//  }

  def validateMin(constraint: NumberConstraints): Rule[JsValue, JsValue] = {

    def isValid(n: JsNumber, minConstraint: Minimum) = {
      if (minConstraint.isExclusive.getOrElse(false)) {
        n.value > minConstraint.min
      } else {
        n.value >= minConstraint.min
      }
    }

    Rule.fromMapping[JsValue, JsValue] {
      case number@JsNumber(_) => constraint.min match {
        case None => Success(number)
        case Some(min) =>
          if (isValid(number, min)) {
            Success(number)
          } else {
            Failure(
              Seq(
                ValidationError("minimum violated." +
                  s"$number must be ${if (min.isExclusive.getOrElse(false)) ">" else ">="} ${min.min}"
                )
              )
            )
          }
      }
      case other => expectedNumber
    }
  }

  def validateMax(constraint: NumberConstraints): Rule[JsValue, JsValue] = {

    def isValid(n: JsNumber, maxConstraint: Maximum) = {
      if (maxConstraint.isExclusive.getOrElse(false)) {
        n.value < maxConstraint.max
      } else {
        n.value <= maxConstraint.max
      }
    }

    Rule.fromMapping[JsValue, JsValue] {
      case number@JsNumber(_) => constraint.max match {
        case None => Success(number)
        case Some(m) =>
          if (isValid(number, m)) {
            Success(number)
          } else {
            Failure(
              Seq(
                ValidationError("max violated",
                  Json.obj("max" -> m.max, "number" -> number)
                )
              )
            )
          }
      }
      case _ => expectedNumber
    }
  }

  def validateMultipleOf(constraint: NumberConstraints): Rule[JsValue, JsValue] = {
    Rule.fromMapping[JsValue, JsValue] {
      case number@JsNumber(n) => constraint.multipleOf match {
        case Some(factor) =>
          if (n.remainder(factor) == BigDecimal(0)) {
            Success(number)
          } else {
            Failure(
              Seq(
                ValidationError("multipleOf violated",
                  Json.obj("factor" -> factor), "number" -> number)
              )
            )
          }
        case None => Success(number)
      }
      case _ => expectedNumber
    }
  }

  private def expectedNumber = Failure(Seq(ValidationError("Expected number")))
}
