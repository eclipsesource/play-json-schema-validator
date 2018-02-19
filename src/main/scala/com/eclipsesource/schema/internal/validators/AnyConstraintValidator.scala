package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.eclipsesource.schema.{SchemaObject, SchemaResolutionContext, SchemaType}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._

import scala.annotation.tailrec
import scalaz.{Failure, Success}

object AnyConstraintValidator {

  def validate(json: JsValue, schema: SchemaType, resolutionContext: SchemaResolutionContext)
              (implicit lang: Lang): VA[JsValue] = {
    val reader: scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] = for {
      allOfRule <- validateAllOf
      anyOfRule <- validateAnyOf
      oneOfRule <- validateOneOf
      enumRule  <- validateEnum
      constRule <- validateConst
      notRule   <- validateNot
      ifThenElseRule <- validateIfThenElse
    } yield allOfRule |+| anyOfRule |+| oneOfRule |+| enumRule |+| constRule |+| notRule |+| ifThenElseRule
    reader
      .run((schema, resolutionContext))
      .repath(_.compose(resolutionContext.instancePath))
      .validate(json)
  }

  def validateIfThenElse(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (schema, context) =>
      Rule.fromMapping { json =>
        (schema.constraints.any._if, schema.constraints.any._then, schema.constraints.any._else) match {
          case (Some(ifSchema), Some(thenSchema), Some(elseSchema)) =>
            val ifSuccess = ifSchema.validate(json, context).isSuccess
            if (ifSuccess && thenSchema.validate(json, context).isSuccess) {
              Success(json)
            } else if (!ifSuccess && elseSchema.validate(json, context).isSuccess) {
              Success(json)
            } else {
              failure(
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
              failure(
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
              failure(
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

  def validateNot(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (schema, context) =>
      Rule.fromMapping { json =>
        schema.constraints.any.not.map(schema =>
          if (schema.validate(json, context).isFailure) {
            Success(json)
          } else {
            failure(
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


  def validateAllOf(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (schema, context) =>
      Rule.fromMapping { json =>
        schema.constraints.any.allOf.map(
          schemas => {
            val mergedSchemas: Seq[SchemaType] = mergeSchemas(schema, schemas)
            val allValidationResults: Seq[VA[JsValue]] = mergedSchemas.map(_.validate(json, context))
            val allMatch = allValidationResults.forall(_.isSuccess)
            if (allMatch) {
              Success(json)
            } else {
              failure(
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

  def validateAnyOf(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] = {

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

    scalaz.Reader { case (schema, context) =>
      Rule.fromMapping {
        json => {
          schema.constraints.any.anyOf match {
            case Some(schemas) =>
              untilFirstSuccess(json, schema, context, schemas.toList, List.empty) match {
                case Nil => Success(json)
                case errors => failure(
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

  def validateOneOf(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (schema, context) =>
      Rule.fromMapping { json =>
        schema.constraints.any.oneOf.map(
          schemas => {
            val mergedSchemas = mergeSchemas(schema, schemas)
            val allValidationResults = mergedSchemas.map(_.validate(json, context))
            allValidationResults.count(_.isSuccess) match {
              case 0 =>
                failure(
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
                failure(
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

  def validateConst(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] = {
    scalaz.Reader { case (schema, context) =>
      Rule.fromMapping { json =>
        schema.constraints.any.const match {
          case Some(constValue) if constValue == json =>
            Success(json)
          case None => Success(json)
          case Some(constValue) => failure(
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

  def validateEnum(implicit lang: Lang): scalaz.Reader[(SchemaType, SchemaResolutionContext), Rule[JsValue, JsValue]] = {
    scalaz.Reader { case (schema, context) =>
      val enums = schema.constraints.any.enum
      Rule.fromMapping { json =>
        enums match {
          case Some(values) if values.contains(json) => Success(json)
          case Some(values) =>
            failure(
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
      // prefix either schemaPath or origin, if it exists
      val prefixedField = obj.fields
        .find(field => field._1 == "origin")
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
