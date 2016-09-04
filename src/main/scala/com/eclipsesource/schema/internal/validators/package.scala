package com.eclipsesource.schema.internal

import com.eclipsesource.schema.internal.validation.Validated
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsPath, JsObject, JsValue, Json}
import scalaz.Failure

package object validators {

  implicit val objectValidator = ObjectValidator

  def failure(msg: String,
              schemaPath: JsPath,
              instancePath: JsPath,
              instance: JsValue,
              additionalInfo: JsObject = Json.obj()
             ): Validated[ValidationError, JsValue] = {
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
