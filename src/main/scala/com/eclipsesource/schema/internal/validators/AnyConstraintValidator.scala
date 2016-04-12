package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaValidator
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.constraints.Constraints.AnyConstraint
import com.eclipsesource.schema.internal.validation.{VA, Rule}
import play.api.libs.json._

import scalaz.{Failure, Success}

object AnyConstraintValidator {

  def validate(json: JsValue, any: AnyConstraint, context: Context): VA[JsValue] = {
    val reader: scalaz.Reader[(AnyConstraint, Context), Rule[JsValue, JsValue]] = for {
      allOfRule <- validateAllOf
      anyOfRule <- validateAnyOf
      oneOfRule <- validateOneOf
      enumRule <- validateEnum
      notRule <- validateNot
    } yield allOfRule |+| anyOfRule |+| oneOfRule |+| enumRule |+| notRule
    reader.run((any, context)).repath(_.compose(context.instancePath)).validate(json)
  }

  def validateNot: scalaz.Reader[(AnyConstraint, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (any ,context) =>
      Rule.fromMapping { json =>
        any.not.map(schema =>
          if (SchemaValidator.validate(schema, json).isError) {
            Success(json)
          } else {
            failure(
              s"$json matches schema '$schema' although it should not.",
              context.schemaPath,
              context.instancePath,
              json
            )
          }
        ).getOrElse(Success(json))
      }
    }

  private def collectFailures(results: Seq[VA[JsValue]], prefix: String): JsObject = {

    def repath(prefix: String)(obj: JsObject): JsObject = {
      val fields = obj.fields.map {
        case ("schemaPath", JsString(schemaPath)) if schemaPath.startsWith("#") => ("schemaPath", JsString(s"#$prefix${schemaPath.drop(1)}"))
        case ("schemaPath", JsString(schemaPath)) => ("schemaPath", JsString(s"$prefix$schemaPath"))
        case field => field
      }
      JsObject(fields)
    }

    results.zipWithIndex.foldLeft(Json.obj()) {
      case (obj, (Failure(errors), idx)) =>
        // TODO: why distinct?
        obj ++ Json.obj(s"$prefix/$idx" ->
          JsArray(
            SchemaUtil.toJson(errors.distinct).value.map {
              case obj: JsObject => repath(s"$prefix/$idx")(obj)
              case js => js
            }
          ))
      case (obj, _) => obj
    }
  }

  def validateAllOf: scalaz.Reader[(AnyConstraint, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (any, context) =>
      Rule.fromMapping { json =>
        any.allOf.map(
          schemas => {
            val allValidationResults: Seq[VA[JsValue]] = schemas.map(schema =>
              SchemaValidator.process(schema, json, context)
            )
            val allMatch = allValidationResults.forall(_.isSuccess)
            if (allMatch) {
              Success(json)
            } else {
              failure(
                s"Instance does not match all schemas",
                context.schemaPath,
                context.instancePath,
                json,
                collectFailures(allValidationResults, "/allOf")
              )
            }
          }
        ).getOrElse(Success(json))
      }
    }

  def validateAnyOf: scalaz.Reader[(AnyConstraint, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (any, context) =>
      Rule.fromMapping { json =>
        any.anyOf.map(
          schemas => {
            val allValidationResults: Seq[VA[JsValue]] = schemas.map(schema =>
              SchemaValidator.process(schema, json, context)
            )
            val maybeSuccess = allValidationResults.find(_.isSuccess)
            maybeSuccess.map(success => Success(json)).getOrElse {
              failure(
                "Instance does not match any of the schemas",
                context.schemaPath,
                context.instancePath,
                json,
                collectFailures(allValidationResults, "/anyOf")
              )
            }
          }
        ).getOrElse(Success(json))
      }
    }

  def validateOneOf: scalaz.Reader[(AnyConstraint, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (any, context) =>
      Rule.fromMapping { json =>
        any.oneOf.map(
          schemas => {

            val allValidationResults = schemas.map( schema =>
              SchemaValidator.process(schema, json, context)
            )
            allValidationResults.count(_.isSuccess) match {
              case 0 =>
                failure(
                  s"Instance does not match any schema",
                  context.schemaPath,
                  context.instancePath,
                  json,
                  collectFailures(allValidationResults, "/oneOf")
                )
              case 1 => Success(json)
              case _ =>
                val matchedPaths = allValidationResults.zipWithIndex.foldLeft(List.empty[String]) {
                  case (arr, (Success(result), idx)) =>
                    arr :+ s"/oneOf/$idx"
                  case (arr, _) => arr
                }
                failure(
                  s"Instance matches more than one schema",
                  context.schemaPath,
                  context.instancePath,
                  json,
                  Json.obj("matched" -> matchedPaths)
                )
            }
          }
        ).getOrElse(Success(json))
      }
    }

  def validateEnum: scalaz.Reader[(AnyConstraint, Context), Rule[JsValue, JsValue]] = {
    scalaz.Reader { case (any, context) =>
      val enums = any.enum
      Rule.fromMapping { json =>
        enums match {
          case Some(values) if values.contains(json) => Success(json)
          case Some(values) =>
            failure(
              "Instance is invalid enum value",
              context.schemaPath,
              context.instancePath,
              json,
              Json.obj(
                "enum" -> values
              )
            )
          case None => Success(json)
        }
      }
    }
  }
}
