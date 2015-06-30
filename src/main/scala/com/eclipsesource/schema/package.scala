package com.eclipsesource

import com.eclipsesource.schema.internal.SchemaUtil
import com.eclipsesource.schema.internal.serialization.{JSONSchemaAnnotationWrites, JSONSchemaReads, JSONSchemaWrites}
import play.api.libs.json._

import scalaz.{Failure => _, Success => _}

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaAnnotationWrites
  with JSONSchemaReads {

  implicit class JsObjectExtensions(jsObject: JsObject) {
    def get(fieldName: String): Option[JsValue] = {
      val jsValue = jsObject \ fieldName
      if (jsValue.isInstanceOf[JsUndefined]) {
        None
      } else {
        Some(jsValue)
      }
    }
  }

  implicit class SchemaTypeExtensionOps(schemaType: SchemaType) {
    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)
  }

  implicit class SchemaObjectExtensionOps(obj: SchemaObject) {
    def hasAttribute(attributeName: String) ={
      obj.properties.exists(_.name == attributeName)
    }
  }
}