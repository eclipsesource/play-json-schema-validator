package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import play.api.libs.json.{JsArray, JsObject, JsString, _}

trait JSONSchemaAnnotationWrites extends JSONSchemaWrites {

  val emptyJsObject = Json.obj()

  override def propertyExtensions = List(
    defaultExtension _,
    readOnlyExtension _)
  override def extensions = List(optionalExtension _)

  def defaultExtension(attr: QBAttribute): JsObject = {
    attr.annotations
      .collectFirst { case default: QBDefaultAnnotation => Json.obj("default" -> default.value) }
      .getOrElse(emptyJsObject)
  }

  def readOnlyExtension(attr: QBAttribute): JsObject = {
    attr.annotations
      .collectFirst { case _: QBReadOnlyAnnotation => Json.obj("readonly" -> "true") }
      .getOrElse(emptyJsObject)
  }

  def optionalExtension(obj: QBClass): JsObject = {
    val required = allNonOptionalAttributes(obj.properties)
    if (required.isEmpty) {
      Json.obj()
    } else {
      Json.obj("required" -> JsArray(required))
    }
  }

  def allNonOptionalAttributes(fields: Seq[QBAttribute]): Seq[JsString] =
  // TODO
    fields.find(_.name == "properties").fold(Seq.empty[JsString])(attr =>
      attr.qbType.as[QBClass].properties
        .filterNot(_.annotations.exists(_.isInstanceOf[QBOptionalAnnotation]))
        .map(_.name).map(JsString)
    )
}

