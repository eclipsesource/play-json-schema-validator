package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{SchemaArray, SchemaObject, SchemaType}
import play.api.libs.json._

object SchemaUtil {

  def prettyPrint(schemaType: SchemaType, indent: Int = 0): String = schemaType match {
    case obj: SchemaObject => "{\n" +
      obj.properties.map { field =>
        " " * (indent + 2)  + field.name + ": " +
          prettyPrint(field.schemaType, indent + 2) + "\n"}.mkString +
        " " * indent + "}"
    case arr: SchemaArray => "[" + prettyPrint(arr.items, indent) + "]"
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
}
