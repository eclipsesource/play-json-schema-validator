package com.eclipsesource.schema.drafts

import com.eclipsesource.schema.internal.draft4.{SchemaReads4, SchemaWrites4}
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.{SchemaConfigOptions, SchemaFormat, SchemaVersion}

trait Version4 extends SchemaVersion with SchemaReads4 with SchemaWrites4

object Version4 extends SchemaVersion with SchemaReads4 with SchemaWrites4 {
  val SchemaUrl = "http://json-schema.org/draft-04/schema#"
  val schemaLocation: String = SchemaUrl
  val options: SchemaConfigOptions = new SchemaConfigOptions {
    override def supportsExternalReferences: Boolean = true
    override def formats: Map[String, SchemaFormat] = DefaultFormats.formats
  }
  def apply(schemaOptions: SchemaConfigOptions): Version4 = {
    new Version4 {
      val schemaLocation: String = SchemaUrl
      override def options: SchemaConfigOptions = schemaOptions
    }
  }
}

