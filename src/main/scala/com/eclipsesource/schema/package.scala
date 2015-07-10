package com.eclipsesource

import com.eclipsesource.schema.internal.SchemaUtil
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}

import scalaz.{Failure => _, Success => _}

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaReads {

  implicit class SchemaTypeExtensionOps(schemaType: SchemaType) {
    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)
  }

}