package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaValidator, SchemaValidator$}
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.constraints.Constraints.AnyConstraint
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsObject, JsValue, Json}

object AnyConstraintValidator {

  def validate(json: JsValue, any: AnyConstraint, context: Context): VA[JsValue] = {
    (validateAllOf(any, context) |+|
      validateAnyOf(any, context) |+|
      validateOneOf(any, context) |+|
      validateEnum(any) |+|
      validateNot(any)
      ).validate(json)
  }

  def validateNot(any: AnyConstraint): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.not.map(schema =>
        if (SchemaValidator.validate(schema, json).isFailure) {
         Success(json)
        } else {
          Failure(
            Seq(
              ValidationError(s"$json matches schema '$schema' although it should not.")
            )
          )
        }
      ).getOrElse(Success(json))
    }
  }

  def validateAllOf(any: AnyConstraint, context: Context): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.allOf.map(
        schemas => {
          val allValidationResults = schemas.map(schema => SchemaValidator.process(schema, json, context))
          val allMatch = allValidationResults.forall(_.isSuccess)
          if (allMatch) {
            Success(json)
          } else {
            Failure(
              Seq(
                ValidationError(s"allOf constraint violated. $json does not match at least one schema at ${context.path}.")
              )
            )
          }
        }
      ).getOrElse(Success(json))
    }
  }

  def validateAnyOf(any: AnyConstraint, context: Context): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.anyOf.map(
        schemas => {
          val allValidationResults = schemas.map(SchemaValidator.validate(_)(json))
          val maybeSuccess = allValidationResults.find(_.isSuccess)
          maybeSuccess.map(success => Success(json)).getOrElse(
            Failure(
              Seq(
                ValidationError(s"anyOf constraint violated. $json does not match any of the schemas at ${context.path}.")
              )
            )
          )
        }
      ).getOrElse(Success(json))
    }
  }

  def validateOneOf(any: AnyConstraint, context: Context): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.oneOf.map(
        schemas => {
          val allValidationResults = schemas.map(schema => SchemaValidator.validate(schema)(json))
          allValidationResults.count(_.isSuccess) match {
            case 0 => Failure(
              Seq(ValidationError(s"oneOf constraint violated. $json does not match any schema at ${context.path}."))
            )
            case 1 => Success(json)
            case _ =>
              Failure(
                Seq(ValidationError(s"oneOf constraint violated. $json does match more than one schema at ${context.path}."))
              )
          }
        }
      ).getOrElse(Success(json))
    }
  }

  def validateEnum(constraints: AnyConstraint): Rule[JsValue, JsValue] = {
    val enums = constraints.enum
    Rule.fromMapping { json =>
        enums match {
          case Some(values) if values.contains(json) => Success(json)
          case Some(values) => Failure(
            Seq(ValidationError(s"enum violated. $json is not part of [${values.mkString(", ")}]"))
          )
          case None => Success(json)
        }
    }
  }

}
