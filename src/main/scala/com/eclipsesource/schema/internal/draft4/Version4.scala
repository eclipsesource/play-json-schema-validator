package com.eclipsesource.schema.internal.draft4

import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.{SchemaConfigOptions, SchemaFormat, SchemaVersion}

trait Version4 extends SchemaVersion with SchemaReads4 with SchemaWrites4

object Version4 extends SchemaVersion with SchemaReads4 with SchemaWrites4 {
  val options = new SchemaConfigOptions {
    override def supportsCanonicalReferencing: Boolean = true
    override def formats: Map[String, SchemaFormat] = DefaultFormats.formats
  }
  def apply(schemaOptions: SchemaConfigOptions): Version4 = {
    new Version4 {
      override def options: SchemaConfigOptions = schemaOptions
    }
  }
}

