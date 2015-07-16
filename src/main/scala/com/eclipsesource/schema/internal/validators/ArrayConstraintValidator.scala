package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.constraints.Constraints.ArrayConstraints
import play.api.data.mapping.{Failure, Rule, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsArray, JsValue, Json}

trait ArrayConstraintValidator {

  def validate(json: JsValue, arrayConstraints: ArrayConstraints): VA[JsValue] = {
    (validateMinItems(arrayConstraints) |+|
      validateMaxItems(arrayConstraints) |+|
      validateUniqueness(arrayConstraints)).validate(json)
  }

  def validateMaxItems(arrayConstraints: ArrayConstraints): Rule[JsValue, JsValue] = {
    val maxItems = arrayConstraints.maxItems
    Rule.fromMapping {
      case json@JsArray(values) => maxItems match {
        case Some(max) => if (values.size <= max) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("maxItems violated",
                Json.obj("maxItems" -> maxItems, "array" -> values)
              )
            )
          )
        }
        case None => Success(json)
      }
      case other => Failure(Seq(ValidationError(s"Expected array, was $other")))
    }
  }

  def validateMinItems(arrayConstraints: ArrayConstraints): Rule[JsValue, JsValue] = {
    val minItems = arrayConstraints.minItems.getOrElse(0)
    Rule.fromMapping {
      case json@JsArray(values) =>
        if (values.size >= minItems) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("minItems violated",
                Json.obj("minItems" -> minItems, "array" -> values)
              )
            )
          )
        }
      case other => Failure(Seq(ValidationError(s"Expected array, was $other")))
    }
  }

  def validateUniqueness(arrayConstraints: ArrayConstraints): Rule[JsValue, JsValue] = {
    val isUnique = arrayConstraints.unique.getOrElse(false)
    Rule.fromMapping {
      case json@JsArray(values) if isUnique =>
        if (values.distinct.size == values.size) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("uniqueItems violated",
                Json.obj("array" -> values)
              )
            )
          )
        }
      case arr@JsArray(_) => Success(arr)
      case other => Failure(Seq(ValidationError(s"Expected array, was $other")))
    }
  }

}
