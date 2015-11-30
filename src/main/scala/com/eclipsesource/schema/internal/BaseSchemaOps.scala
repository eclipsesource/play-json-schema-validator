package com.eclipsesource.schema.internal

import scala.annotation.tailrec

import com.eclipsesource.schema._

/**
 * Primitive schema functions.
 */
trait BaseSchemaOps {

  /**
   * Merges the attributes of the second schema into the first one.
   *
   * @param schema
   *             the target schema
   * @param otherSchema
   *             the schema to be merged into the target schema
   *
   * @return the merged schema object definition
   */
  // TODO: duplicate check
  def merge(schema: SchemaObject, otherSchema: SchemaObject) = {
    val fieldNames = otherSchema.properties.map(_.name)
    schema.copy(properties = schema.properties.filterNot(fd => fieldNames.contains(fd.name)) ++ otherSchema.properties)
  }
}
