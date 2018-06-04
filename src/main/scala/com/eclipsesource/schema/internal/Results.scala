package com.eclipsesource.schema.internal

import com.eclipsesource.schema.SchemaResolutionContext
import com.eclipsesource.schema.internal.validation.VA
import play.api.libs.json._

import scalaz.{Failure, Success}

object Results {

  def merge(va1: VA[JsValue], va2: VA[JsValue]): VA[JsValue] = {
    (va1, va2) match {
      case (Success(_), f@Failure(_)) => f
      case (f@Failure(_), Success(_)) => f
      case (f1@Failure(errs1), f2@Failure(errs2)) => Failure(errs1 ++ errs2)
      case (s@Success(json), Success(_)) => s
    }
  }

  def aggregateAsObject(validatedProps: Seq[(String, VA[JsValue])], context: SchemaResolutionContext): VA[JsValue] = {
    validatedProps.foldLeft[VA[JsValue]](Success(Json.obj()))((va, result) => (va, result._2) match {
      case (Success(_), f@Failure(_)) => f
      case (f@Failure(_), Success(_)) => f
      case (Failure(errs1), Failure(errs2)) => Failure(errs1 ++ errs2)
      case (Success(obj@JsObject(_)), Success(s2)) => Success(JsObject(obj.fields :+ (result._1, s2)))
      case (_, _) => va
    })
  }

  def success(prop: (String, JsValue)): PropertyValidationResult = prop._1 -> Success(prop._2)

  def failureWithPath(keyword: String,
                      msg: String,
                      context: SchemaResolutionContext,
                      instance: JsValue,
                      additionalInfo: JsObject = Json.obj()): VA[JsValue] = {

    Failure(Seq(context.instancePath ->
      Seq(JsonValidationError(
        msg,
        SchemaUtil.createErrorObject(keyword, context.schemaPath, context.instancePath, instance, additionalInfo)
          ++ context.scope.id.fold(Json.obj())(id => Json.obj("resolutionScope" -> id.value))
          ++ context.scope.referrer.fold(Json.obj())(referrer => Json.obj("referrer" -> SchemaUtil.dropSlashIfAny(referrer.toString())))
      ))
    ))
  }

}
