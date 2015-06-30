package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import play.api.libs.json.{JsArray, JsObject, JsString, _}

trait JSONSchemaAnnotationWrites extends JSONSchemaWrites {

  val emptyJsObject = Json.obj()

  override def propertyExtensions = List(
    defaultExtension _,
    readOnlyExtension _)
  override def extensions = List(optionalExtension _)

  def defaultExtension(attr: SchemaAttribute): JsObject = {
    attr.annotations
      .collectFirst { case default: SchemaDefaultAnnotation => Json.obj("default" -> default.value) }
      .getOrElse(emptyJsObject)
  }

  def readOnlyExtension(attr: SchemaAttribute): JsObject = {
    attr.annotations
      .collectFirst { case _: SchemaReadOnlyAnnotation => Json.obj("readonly" -> "true") }
      .getOrElse(emptyJsObject)
  }

  def optionalExtension(obj: SchemaObject): JsObject = {
    val required = allNonOptionalAttributes(obj.properties)
    if (required.isEmpty) {
      Json.obj()
    } else {
      Json.obj("required" -> JsArray(required))
    }
  }

  def allNonOptionalAttributes(fields: Seq[SchemaAttribute]): Seq[JsString] =
  // TODO
    fields.find(_.name == "properties").fold(Seq.empty[JsString])(attr =>
      attr.schemaType.asInstanceOf[SchemaObject].properties
        .filterNot(_.annotations.exists(_.isInstanceOf[SchemaOptionalAnnotation]))
        .map(_.name).map(JsString)
    )
}

