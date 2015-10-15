package com.eclipsesource.schema.internal

import play.api.data.mapping.{VA, Failure, Validation}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsValue, Json}

package object validators {

  implicit val objectValidator = ObjectValidator

  def failure(msg: String, schemaPath: String, instancePath: String, instance: JsValue, subErrors: Option[Seq[VA[JsValue]]] = None): Validation[ValidationError, JsValue] = {
    Failure(
      Seq(ValidationError(msg,
        Json.obj(
          "schemaPath" -> schemaPath,
          "instancePath" -> instancePath,
          "value" -> instance
        ) ++ subErrors.fold(Json.obj())(errors =>
          Json.obj("errors" -> errors.collect { case Failure(error) => SchemaUtil.toJson(error) })
        )
      ))
    )
  }


}
