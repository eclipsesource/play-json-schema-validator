package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{SchemaArray, SchemaObject, SchemaType}
import play.api.data.mapping.{Path, ValidationError}
import play.api.libs.json._

object SchemaUtil {

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

  def toJson(errors:  Seq[(Path, Seq[ValidationError])]): JsArray = {
    val emptyErrors = Json.arr()
    errors.foldLeft(emptyErrors) { case (accumulatedErrors, (path, validationErrors)) =>
      val maybeError = validationErrors.foldLeft(None: Option[JsObject])((aggregatedError, err) => err.args.headOption match {
        case Some(o@JsObject(fields)) =>
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
