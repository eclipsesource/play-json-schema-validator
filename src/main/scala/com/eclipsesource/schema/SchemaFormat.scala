package com.eclipsesource.schema

import play.api.libs.json.JsValue

trait SchemaFormat {
  /**
    * The name of the format.
    * @return the format name
    */
  def name: String

  /**
    * Check whether the given value conforms to this format.
    *s
    * @param json the JSON value to be checked
    * @return whether the JSON value conforms to this format
    */
  def validate(json: JsValue): Boolean
}
