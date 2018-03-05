package com.eclipsesource.schema

import com.eclipsesource.schema.internal.serialization.{SchemaReads, SchemaWrites}

trait SchemaVersion extends SchemaReads with SchemaWrites {
  def schemaLocation: String
  def options: SchemaConfigOptions
}
