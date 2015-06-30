package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.Validator
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.constraints.Constraints.AnyConstraint
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsObject, JsValue, Json}

object AnyConstraintValidator {

  def validate(json: JsValue, any: AnyConstraint, context: Context): VA[JsValue] = {
    (validateAllOf(any, context) |+| validateAnyOf(any) |+| validateOneOf(any)).validate(json)
  }

  def validateAllOf(any: AnyConstraint, context: Context): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.allOf.map(
        schemas => {
          val allValidationResults = schemas.map(schema => Validator.process(schema, json, context))
          val allMatch = allValidationResults.forall(_.isSuccess)
          if (allMatch) {
            Success(json)
          } else {
            Failure(
              Seq(
                ValidationError("allOf violated",
                  Json.obj("schemas" -> Json.toJson(schemas.map(_.prettyPrint)), "object" -> json)
                )
              )
            )
          }
        }
      ).getOrElse(Success(json))
    }
  }

  def validateAnyOf(any: AnyConstraint): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.anyOf.map(
        schemas => {
          val allValidationResults = schemas.map(Validator.validate(_)(json))
          val maybeSuccess = allValidationResults.find(_.isSuccess)
          maybeSuccess.map(success => Success(json)).getOrElse(
            Failure(
              Seq(
                ValidationError("anyOf violated",
                  Json.obj("schemas" -> Json.arr(schemas.map(_.prettyPrint)), "object" -> json)
                )
              )
            )
          )
        }
      ).getOrElse(Success(json))
    }
  }

  def validateOneOf(any: AnyConstraint): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.oneOf.map(
        schemas => {
          val allValidationResults = schemas.map(schema => Validator.validate(schema)(json))
          allValidationResults.count(_.isSuccess) match {
            case 0 => Failure(
              Seq(
                ValidationError("oneOf violated (none applies)",
                  Json.obj("schemas" -> Json.arr(schemas.map(_.prettyPrint)), "object" -> json)
                )
              )
            )
            case 1 => Success(json)
            case _ =>
              Failure(
                Seq(
                  ValidationError("oneOf violated (multiple apply)",
                    Json.obj("schemas" -> Json.arr(schemas.map(_.prettyPrint)), "object" -> json)
                  )
                )
              )
          }
        }
      ).getOrElse(Success(json))
    }
  }

}
