package com.eclipsesource.schema

trait SchemaStringFormat {
  /**
    * The name of the format.
    * @return the format name
    */
  def name: String

  /**
    * Check whether the given string conforms to this format
    * @param s the string to be checked
    * @return whether the string conforms to this format
    */
  def validate(s: String): Boolean
}

