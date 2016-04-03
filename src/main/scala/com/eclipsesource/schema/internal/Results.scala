package com.eclipsesource.schema.internal

import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json._

object Results {

  def merge(va1: VA[JsValue], va2: VA[JsValue]): VA[JsValue] = {
    (va1, va2) match {
      case (Success(_), f@Failure(err)) => f
      case (f@Failure(_), Success(_)) => f
      case (f1@Failure(errs1), f2@Failure(errs2)) => Failure(errs1 ++ errs2)
      case (s@Success(obj1@JsObject(_)), Success(_)) => s
      case (s@Success(JsArray(values1)), Success(_)) => s
      case (s@Success(json), Success(_)) => s
    }
  }

  def aggregateAsObject(validatedProps: Seq[(String, VA[JsValue])], context: Context): VA[JsValue] = {
    validatedProps.foldLeft[VA[JsValue]](Success(Json.obj()))((va, result) => (va, result._2) match {
      case (Success(_), f@Failure(err)) => f
      case (f@Failure(_), Success(_)) => f
      case (f1@Failure(errs1), f2@Failure(errs2)) => Failure(errs1 ++ errs2)
      case (Success(obj@JsObject(fields)), Success(s2)) => Success(JsObject(obj.fields :+ (result._1, s2)))
    })
  }

  def success(prop: (String, JsValue)): PropertyValidationResult = {
    prop._1 -> Success(prop._2)
  }

  def failureWithPath(msg: String,
                      schemaPath: Path,
                      instancePath: Path,
                      instance: JsValue,
                     additionalInfo: JsObject = Json.obj()): VA[JsValue] = {
    // TODO: generalize
    def dropSlashIfAny(path: String) = if (path.startsWith("/#")) path.substring(1) else path
    Failure(Seq(instancePath ->
      Seq(ValidationError(msg,
         Json.obj(
          "schemaPath" -> dropSlashIfAny(schemaPath.toString()),
          "instancePath" -> instancePath.toString(),
          "value" -> instance,
          "errors" ->  additionalInfo
        )
      ))
    ))
  }

}
