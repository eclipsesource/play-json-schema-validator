package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.eclipsesource.schema.{SchemaObject, SchemaResolutionContext, SchemaType}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._

import scala.annotation.tailrec
import scalaz.{Failure, Success}

object AnyConstraintValidators {

  def validateIfThenElse(_if: Option[SchemaType], _then: Option[SchemaType], _else: Option[SchemaType])
                        (implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping { json =>
        (_if, _then, _else) match {
          case (Some(ifSchema), Some(thenSchema), Some(elseSchema)) =>
            val ifSuccess = ifSchema.validate(json, context).isSuccess
            if (ifSuccess && thenSchema.validate(json, context).isSuccess) {
              Success(json)
            } else if (!ifSuccess && elseSchema.validate(json, context).isSuccess) {
              Success(json)
            } else {
              SchemaUtil.failure(
                "else",
                Messages("err.if.then.else", json),
                context.schemaPath,
                context.instancePath,
                json
              )
            }
          case (Some(ifSchema), Some(thenSchema), None) =>
            val ifSuccess = ifSchema.validate(json, context).isSuccess
            if (ifSuccess && thenSchema.validate(json, context).isSuccess) {
              Success(json)
            } else if (!ifSuccess) {
              Success(json)
            } else {
              SchemaUtil.failure(
                "then",
                Messages("err.if.then.else", json),
                context.schemaPath,
                context.instancePath,
                json
              )
            }
          case (Some(ifSchema), None, Some(elseSchema)) =>
            if (ifSchema.validate(json, context).isSuccess) {
              Success(json)
            } else if (elseSchema.validate(json, context).isSuccess) {
              Success(json)
            } else {
              SchemaUtil.failure(
                "else",
                Messages("err.if.then.else", json),
                context.schemaPath,
                context.instancePath,
                json
              )
            }
          case (Some(_), None, None) => Success(json)
          case (None, _, _) => Success(json)
        }
      }
    }

  def validateNot(not: Option[SchemaType])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping { json =>
        not.map(schema =>
          if (schema.validate(json, context).isFailure) {
            Success(json)
          } else {
            SchemaUtil.failure(
              Keywords.Any.Not,
              Messages("any.not", json),
              context.schemaPath,
              context.instancePath,
              json
            )
          }
        ).getOrElse(Success(json))
      }
    }


  def validateAllOf(schema: SchemaType, allOf: Option[Seq[SchemaType]])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping { json =>
        allOf.map(
          schemas => {
            val mergedSchemas: Seq[SchemaType] = mergeSchemas(schema, schemas)
            val allValidationResults: Seq[VA[JsValue]] = mergedSchemas.map(_.validate(json, context))
            val allMatch = allValidationResults.forall(_.isSuccess)
            if (allMatch) {
              Success(json)
            } else {
              SchemaUtil.failure(
                Keywords.Any.AllOf,
                Messages("any.all"),
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

  def validateAnyOf(schema: SchemaType, anyOf: Option[Seq[SchemaType]])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {

    @tailrec
    def untilFirstSuccess(json: JsValue, baseSchema: SchemaType, context: SchemaResolutionContext,
                          schemas: List[SchemaType], results: List[VA[JsValue]]): List[VA[JsValue]] = schemas match {
      case s::ss =>
        val mergedSchema = mergeSchema(s, baseSchema)
        mergedSchema.validate(json, context) match {
          case Success(_) => Nil
          case failure@Failure(_) => untilFirstSuccess(json, baseSchema, context, ss, failure :: results)
        }
      case Nil => results.reverse
    }

    scalaz.Reader { context =>
      Rule.fromMapping {
        json => {
          anyOf match {
            case Some(schemas) =>
              untilFirstSuccess(json, schema, context, schemas.toList, List.empty) match {
                case Nil => Success(json)
                case errors => SchemaUtil.failure(
                  Keywords.Any.AnyOf,
                  Messages("any.any"),
                  context.schemaPath,
                  context.instancePath,
                  json,
                  collectFailures(errors, "/anyOf")
                )
              }
            case None => Success(json)
          }
        }
      }
    }
  }

  def validateOneOf(schema: SchemaType, oneOf: Option[Seq[SchemaType]])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping { json =>
        oneOf.map(
          schemas => {
            val mergedSchemas = mergeSchemas(schema, schemas)
            val allValidationResults = mergedSchemas.map(_.validate(json, context))
            allValidationResults.count(_.isSuccess) match {
              case 0 =>
                SchemaUtil.failure(
                  Keywords.Any.OneOf,
                  Messages("any.one.of.none"),
                  context.schemaPath,
                  context.instancePath,
                  json,
                  collectFailures(allValidationResults, "/oneOf")
                )
              case 1 => Success(json)
              case _ =>
                val matchedPaths = allValidationResults.zipWithIndex.foldLeft(List.empty[String]) {
                  case (arr, (Success(_), idx)) =>
                    arr :+ s"/oneOf/$idx"
                  case (arr, _) => arr
                }
                SchemaUtil.failure(
                  Keywords.Any.OneOf,
                  Messages("any.one.of.many"),
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

  def validateConst(const: Option[JsValue])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {
    scalaz.Reader { context =>
      Rule.fromMapping { json =>
        const match {
          case Some(constValue) if constValue == json =>
            Success(json)
          case None => Success(json)
          case Some(constValue) => SchemaUtil.failure(
            "const",
            Messages("any.const"),
            context.schemaPath,
            context.instancePath,
            json,
            Json.obj("const" -> constValue)
          )
        }
      }
    }
  }

  def validateEnum(enum: Option[Seq[JsValue]])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {
    scalaz.Reader { context =>
      val enums = enum
      Rule.fromMapping { json =>
        enums match {
          case Some(values) if values.contains(json) => Success(json)
          case Some(values) =>
            SchemaUtil.failure(
              Keywords.Any.Enum,
              Messages("any.enum"),
              context.schemaPath,
              context.instancePath,
              json,
              Json.obj("enum" -> values)
            )
          case None => Success(json)
        }
      }
    }
  }

  private def collectFailures(results: Seq[VA[JsValue]], prefix: String): JsObject = {

    def repath(prefix: String)(obj: JsObject): JsObject = {
      // prefix either schemaPath or referrer, if it exists
      val prefixedField = obj.fields
        .find(field => field._1 == "referrer")
        .orElse(obj.fields.find(field => field._1 == "schemaPath"))
        .map {
          case (name, JsString(schemaPath)) if schemaPath.startsWith("#") =>
            (name, JsString(s"#$prefix${schemaPath.drop(1)}"))
        }
      prefixedField.fold(obj)(field => {
        val index = obj.fields.indexWhere(_._1 == field._1)
        JsObject(obj.fields.updated(index, field))
      })
    }

    results.zipWithIndex.foldLeft(Json.obj()) {
      case (obj, (Failure(errors), idx)) =>
        obj ++ Json.obj(s"$prefix/$idx" ->
          JsArray(
            SchemaUtil.toJson(errors).value.map {
              case obj: JsObject => repath(s"$prefix/$idx")(obj)
              case js => js
            }
          ))
      case (obj, _) => obj
    }
  }

  private def mergeSchema(schema: SchemaType, otherSchema: SchemaType): SchemaType = (schema, otherSchema) match {
    case (s1: SchemaObject, s2: SchemaObject) => s1.copy(properties = s1.properties ++ s2.properties)
    case _ => schema
  }

  private def mergeSchemas(schema: SchemaType, subSchemas: Seq[SchemaType]): Seq[SchemaType]  = schema match {
    case obj: SchemaObject =>
      subSchemas.map {
        case otherObj: SchemaObject => otherObj.copy(properties = obj.properties ++ otherObj.properties)
        case other => other
      }
    case _ => subSchemas
  }
}
