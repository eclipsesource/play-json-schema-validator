package com.eclipsesource.schema

import play.api.libs.json._

trait ErrorHelper {

  def firstErrorOf[A](res: JsResult[A]): String = {
    val errors: Seq[(JsPath, Seq[JsonValidationError])] = res.asEither.left.get
    val firstError: JsValue = errors.toJson(0)
    (firstError \ "msgs").get.as[JsArray].head.as[String]
  }
}
