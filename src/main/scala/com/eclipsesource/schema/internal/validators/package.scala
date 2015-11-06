package com.eclipsesource.schema.internal

import play.api.data.mapping.{VA, Failure, Validation}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsArray, JsValue, Json}

package object validators {

  implicit val objectValidator = ObjectValidator

  def failure(msg: String, schemaPath: String, instancePath: String,
              instance: JsValue, subErrors: Option[Seq[VA[JsValue]]] = None): Validation[ValidationError, JsValue] = {
    val errors: JsArray = subErrors
      .map(errors => JsArray(errors.collect { case Failure(error) => SchemaUtil.toJson(error) }))
      .getOrElse(JsArray())
    Failure(
      Seq(
        ValidationError(msg,
          Json.obj(
            "schemaPath" -> schemaPath,
            "instancePath" -> instancePath,
            "value" -> instance,
            "errors" -> errors
          )
        )
      )
    )
  }
}
