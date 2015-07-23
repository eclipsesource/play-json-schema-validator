package com.eclipsesource.schema.internal

import play.api.data.mapping.{VA, Path, Success, Failure}
import play.api.data.validation.ValidationError
import play.api.libs.json._

object Results {

//  def aggregateResults(validatedProps: Seq[PropertyValidationResult], context: Context): ValidationStatus = {
//    validatedProps.foldLeft(ValidationStatus.empty)((status, prop) => prop._2 match {
//      case Failure(err) => status.addToInvalidProps(err)
//      case Success(JsAbsent) => status
//      case Success(undefined: JsUndefined) => status.addToInvalidProps(
//        (context.path \ prop._1, Seq(ValidationError(undefined.error)))
//      )
//      case Success(value) => status.addToValidProps(prop._1 -> value)
//    })
//  }

  def merge(va1: VA[JsValue], va2: VA[JsValue]): VA[JsValue] = {
    (va1, va2) match {
      case (Success(_), f@Failure(err)) => f
      case (f@Failure(_), Success(_)) => f
      case (f1@Failure(errs1), f2@Failure(errs2)) => Failure(errs1 ++ errs2)
      case (Success(obj1@JsObject(_)), Success(obj2@JsObject(_))) => Success(obj1 ++ obj2)
      case (Success(JsArray(values1)), Success(JsArray(values2))) => Success(JsArray(values1 ++ values2))
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

  def mergeAsObject(valid: Seq[(String, JsValue)], invalid: Seq[VA[JsValue]]): VA[JsValue] = {
    if (invalid.nonEmpty) {
      invalid.collect { case f@Failure(_) => f }.reduceLeft((f1, f2) => Failure.merge(f1, f2))
    } else {
      Success(JsObject(valid))
    }
  }

  def failure(errorMsg: String): VA[JsValue] = {
    Failure(Seq(Path -> Seq(ValidationError(errorMsg))))
  }

  def failure(error: ValidationError): VA[JsValue] = {
    Failure(Seq(Path -> Seq(error)))
  }

  def failure(path: Path, errorMsg: String): VA[JsValue] = {
    Failure(Seq(path -> Seq(ValidationError(errorMsg))))
  }

  def failure(path: Path, error: ValidationError): VA[JsValue] = {
    Failure(Seq(path -> Seq(error)))
  }

  def success(prop: (String, JsValue)): PropertyValidationResult = {
    prop._1 -> Success(prop._2)
  }
}
