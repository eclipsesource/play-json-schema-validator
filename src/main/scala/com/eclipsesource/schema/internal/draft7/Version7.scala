package com.eclipsesource.schema.internal.draft7

import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.{SchemaConfigOptions, SchemaFormat, SchemaVersion}

trait Version7 extends SchemaVersion with SchemaReads7 with SchemaWrites7

object Version7 extends Version7 {
  private[schema] val SchemaUrl = "http://json-schema.org/draft-07/schema#"
  val schemaLocation: String = SchemaUrl
  val options: SchemaConfigOptions = new SchemaConfigOptions {
    override def supportsCanonicalReferencing: Boolean = false
    // TODO
    override def formats: Map[String, SchemaFormat] = DefaultFormats.formats
  }

  def apply(schemaOptions: SchemaConfigOptions): Version7 = {
    new Version7 {
      val schemaLocation: String = SchemaUrl
      override def options: SchemaConfigOptions = schemaOptions
    }
  }
}
