package com.eclipsesource.schema.internal

import com.eclipsesource.schema.internal.validation.Validated
import com.eclipsesource.schema.{SchemaArray, SchemaObject, SchemaType}
import play.api.libs.json._

import scalaz.Failure

object SchemaUtil {

  def failure(keyword: String,
              msg: String,
              schemaPath: JsPath,
              instancePath: JsPath,
              instance: JsValue,
              additionalInfo: JsObject = Json.obj()
             ): Validated[JsonValidationError, JsValue] = {

    def dropSlashIfAny(path: String) = if (path.startsWith("/#")) path.substring(1) else path

    Failure(
      Seq(
        JsonValidationError(msg,
          Json.obj(
            "keyword" -> keyword,
            "schemaPath" -> dropSlashIfAny(schemaPath.toString()),
            "instancePath" -> instancePath.toString(),
            "value" -> instance,
            "errors" ->  additionalInfo
          )
        )
      )
    )
  }

  def prettyPrint(schemaType: SchemaType, indent: Int = 0): String = schemaType match {
    case obj: SchemaObject => "{\n" +
      obj.properties.map { field =>
        " " * (indent + 2)  + field.name + ": " +
          prettyPrint(field.schemaType, indent + 2) + "\n"}.mkString +
        " " * indent + "}"
    case arr: SchemaArray => "[" + prettyPrint(arr.item, indent) + "]"
    case other => other.toString
  }

  def typeOfAsString(json: JsValue): String = {
    json match {
      case JsString(_)  => "string"
      case JsNumber(_)  => "number"
      case JsBoolean(_) => "boolean"
      case JsObject(_)  => "object"
      case JsArray(_)   => "array"
      case JsNull   => "null"
    }
  }

  def toJson(errors:  Seq[(JsPath, Seq[JsonValidationError])]): JsArray = {
    val emptyErrors = Json.arr()
    errors.foldLeft(emptyErrors) { case (accumulatedErrors, (_, validationErrors)) =>
      val maybeError = validationErrors.foldLeft(None: Option[JsObject])((aggregatedError, err) => err.args.headOption match {
        case Some(o@JsObject(_)) =>
          Some(
            aggregatedError.fold(
              deepMerge(o, Json.obj("msgs" -> err.messages))
            )(errObj => deepMerge(errObj, Json.obj("msgs" -> err.messages)))
          )
        case _ => aggregatedError
      })
      maybeError.fold(accumulatedErrors)(o => accumulatedErrors :+ o)
    }
  }

  private def deepMerge(obj: JsObject, other: JsObject): JsObject = {
    def merge(existingObject: JsObject, otherObject: JsObject): JsObject = {
      val result = existingObject.fields.toMap ++ otherObject.fields.toMap.map {
        case (otherKey, otherValue) =>
          val maybeExistingValue = existingObject.fields.toMap.get(otherKey)

          val newValue = (maybeExistingValue, otherValue) match {
            case (Some(e: JsObject), o: JsObject) => merge(e, o)
            case (Some(e: JsArray), o: JsArray) => e ++ o
            case _ => otherValue
          }
          otherKey -> newValue
      }
      JsObject(result)
    }
    merge(obj, other)
  }

}
