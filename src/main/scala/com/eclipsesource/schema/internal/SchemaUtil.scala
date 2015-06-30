package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{SchemaArray, SchemaObject, SchemaType}
import play.api.libs.json._

object SchemaUtil {

  def isNotNullOrUndefined(input: JsValue) = input match {
    case _: JsNull.type => false
    case _: JsUndefined => false
    case _ => true
  }

  /**
   * Used for formatting error messages.
   *
   * @param jsValue
   *           the JsValue which to create a string representation for
   * @return a string representation of the given JsValue
   */
  def mapJsValueToTypeName(jsValue: JsValue): String = jsValue match {
    case _: JsNumber => "number"
    case _: JsString => "string"
    case _: JsBoolean => "boolean"
    case _: JsObject => "object"
    case _: JsArray => "array"
    case _: JsUndefined => "undefined"
    case JsNull => "null"
    case _ => "<no type>"
  }


  def prettyPrint(schemaType: SchemaType, printAnnotations: Boolean = false, indent: Int = 0): String = schemaType match {
    case obj: SchemaObject => "{\n" +
      obj.properties.map { field =>
        " " * (indent + 2)  + field.name + ": " + (if (printAnnotations) field.annotations.mkString(",") else "") +
          prettyPrint(field.schemaType, printAnnotations, indent + 2) + "\n"}.mkString +
        " " * indent + "}"
    case arr: SchemaArray => "[" + prettyPrint(arr.items, printAnnotations, indent) + "]"
    case q => q.toString
  }
}
