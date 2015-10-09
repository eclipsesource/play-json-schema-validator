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
      validateNot(any, context)
      ).validate(json)
  }

  def validateNot(any: AnyConstraint, context: Context): Rule[JsValue, JsValue] = {
    Rule.fromMapping { json =>
      any.not.map(schema =>
        if (SchemaValidator.validate(schema, json).isFailure) {
          Success(json)
        } else {
          Results.failure2(
            s"$json matches schema '$schema' although it should not.",
            context.schemaPath.toString(),
            context.instancePath.toString(),
            schema,
            json
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
            Results.failure2(
              s"$json does not match all schemas",
              context.schemaPath.toString(),
              context.instancePath.toString(),
              context.root,
              json
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
            Results.failure2(
              s"$json does not match any of the schemas",
              context.schemaPath.toString(),
              context.instancePath.toString(),
              context.root,
              json
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
            case 0 =>
              Results.failure2(
                s"$json does not match any schema",
                context.schemaPath.toString(),
                context.instancePath.toString(),
                context.root,
                json
              )
            case 1 => Success(json)
            case _ =>
              Results.failure2(
                s"$json does match more than one schema",
                context.schemaPath.toString(),
                context.instancePath.toString(),
                context.root,
                json
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
