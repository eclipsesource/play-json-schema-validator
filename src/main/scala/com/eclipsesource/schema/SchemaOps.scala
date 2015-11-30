package com.eclipsesource.schema

import com.eclipsesource.schema.internal.BaseSchemaOps
import play.api.libs.json.{JsObject, JsValue}

import scala.reflect.ClassTag

/**
 * Contains all schema operations.
 *
 * Generally, schema operations fail with an exception if an error occurs, since reasonable
 * error handling in most cases isn't possible.
 */
trait SchemaOps extends BaseSchemaOps { self =>

  /**
   * ----------------------------------------------------------
   * 	Schema Ops Extensions
   * ----------------------------------------------------------
   */
  implicit class SchemaObjectOps(schema: SchemaObject) {

    /**
     * Merges the attributes of the given schema into this schema.
     *
     * @param otherSchema
     *           the schema to be merged with this schema
     *
     * @return the updated schema containing the additional attributes from the other schema
     */
    def ++(otherSchema: SchemaObject): SchemaObject = merge(schema, otherSchema)
  }
}
