package com.eclipsesource.schema.internal

import play.api.data.mapping.{Path, VA, Failure, Validation}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsArray, JsValue, Json}

package object validators {

  implicit val objectValidator = ObjectValidator

  def failure(msg: String, schemaPath: Path, instancePath: Path,
              instance: JsValue, subErrors: Seq[String] = Seq.empty): Validation[ValidationError, JsValue] = {
    Failure(
      Seq(
        ValidationError(msg,
          Json.obj(
            "schemaPath" -> schemaPath.toString(),
            "instancePath" -> instancePath.toString(),
            "value" -> instance,
            "errors" -> subErrors
          )
        )
      )
    )
  }
}
