package com.eclipsesource.schema.internal

import play.api.data.mapping.{Failure, Path, Validation}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsObject, JsValue, Json}

package object validators {

  implicit val objectValidator = ObjectValidator

  def failure(msg: String,
              schemaPath: Path,
              instancePath: Path,
              instance: JsValue,
              additionalInfo: JsObject = Json.obj()
             ): Validation[ValidationError, JsValue] = {
    def dropSlashIfAny(path: String) = if (path.startsWith("/#")) path.substring(1) else path
    Failure(
      Seq(
        ValidationError(msg,
          Json.obj(
            "schemaPath" -> dropSlashIfAny(schemaPath.toString()),
            "instancePath" -> instancePath.toString(),
            "value" -> instance,
            "errors" ->  additionalInfo
          )
        )
      )
    )
  }
}
