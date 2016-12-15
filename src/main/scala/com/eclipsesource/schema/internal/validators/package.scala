package com.eclipsesource.schema.internal

import com.eclipsesource.schema.internal.validation.Validated
import play.api.libs.json._

import scalaz.Failure

package object validators {

  implicit val objectValidator = ObjectValidator

  def failure(keyword: String,
              msg: String,
              schemaPath: JsPath,
              instancePath: JsPath,
              instance: JsValue,
              additionalInfo: JsObject = Json.obj()
             ): Validated[JsonValidationError, JsValue] = {

    def dropSlashIfAny(path: String) = if (path.startsWith("/#")) path.substring(1) else path

    Failure(
      Seq(
      JsonValidationError(msg,
        Json.obj(
          "keyword" -> keyword,
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
