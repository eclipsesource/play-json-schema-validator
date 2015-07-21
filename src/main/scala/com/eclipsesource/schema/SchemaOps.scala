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
     * Resolves the given path on this schema.
     *
     * @param path
     *           the path to be resolved
     * @tparam A
     *           the expected type
     * @return the resolved sub-schema
     */
    // TODO: provider without type parameter
    def resolve[A <: SchemaType](path: SchemaPath): A = resolvePath[A](schema)(path)

    /**
     * Renames the attribute located at the given path.
     * <br>
     * Example: Given a schema <code>{ a: { b: int } }</code>,
     * <code>rename("a.b", "c")</code> will change the object to
     * <code>{ a: { c: int }}</code>.
     *
     * @param path
     *             the complete path to the attribute to be renamed
     * @param newAttributeName
     *             the new name of the attribute
     *
     * @return the updated schema containing the renamed attribute
     */
    def rename(path: SchemaPath, newAttributeName: String): SchemaObject =
      renameAttribute(schema)(path, newAttributeName)

    /**
     * Renames a attribute.
     *
     * <br>
     * Example: Given a schema <code>{ a: { b: int } }</code>,
     * <code>rename("a", "c")</code> will change the object to
     * <code>{ c: { b: int }}</code>.
     *
     * @param oldAttributeName
     *              the attribute to be renamed
     * @param newAttributeName
     *              the new name of the attribute
     *
     * @return the updated schema containing the renamed attribute
     */
    def rename(oldAttributeName: String, newAttributeName: String): SchemaObject =
      renameAttribute(schema)(oldAttributeName, newAttributeName)

    /**
     * Retains all given attribute names of this schema at the given path.
     *
     * @param path
     *             the path to the attributes to be kept
     * @param attributeNames
     *             the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes at the given path
     */
    def keep(path: SchemaPath, attributeNames: List[String]): SchemaObject =
      retain(schema)(path, attributeNames)

    /**
     * Synonym for keep(SchemaPath, List[String]).
     *
     * @param path
     *         the path to the attributes to be kept
     * @param attributeNames
     *         the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes at the given path
     */
    def keepAt(path: SchemaPath, attributeNames: String*): SchemaObject =
      retain(schema)(path, attributeNames)

    /**
     * Retains all given attributes of this schema.
     *
     * @param attributes
     *         the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes
     */
    def keep(attributes: String*): SchemaObject = retain(schema)("", attributes)

    /**
     * Retains all fields of this schema
     *
     * @param attributes
     *          the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes
     */
    def keep(attributes: List[String]): SchemaObject = retain(schema)("", attributes)

    /**
     * Adds the given attribute at the path of this schema.
     *
     * @param path
     *             the path at which the new attribute should be inserted
     * @param attr
     *             the attribute to be added
     *
     * @return the updated schema containing the additional attribute
     */
    def +(path: SchemaPath, attr: SchemaAttribute): SchemaObject = add(schema)(path, List(attr))

    /**
     * Adds the given attribute to the root of this schema.
     *
     * @param attr
     *             the attribute to be added
     *
     * @return the updated schema containing the additional attribute
     */
    def +(attr: SchemaAttribute): SchemaObject = add(schema)("", List(attr))

    /**
     * Merges the attributes of the given schema into this schema.
     *
     * @param otherSchema
     *           the schema to be merged with this schema
     *
     * @return the updated schema containing the additional attributes from the other schema
     */
    def ++(otherSchema: SchemaObject): SchemaObject = merge(schema, otherSchema)

    /**
     * Adds the given attributes at the path of the this schema.
     * 
     * @param path
     *           the path at which to insert the attributes
     * @param attributes
     *           the attributes to be added
     *
     * @return the updated schema containing the additional attributes at the specified path
     */
    def ++(path: String, attributes: SchemaAttribute*): SchemaObject = add(schema)(path, attributes.toList)

    /**
     * Adds the given attributes to the root of this schema.
     *
     * @param attributes
     *            the attributes to be added
     *
     * @return the updated schema containing the additional attributes at the root level
     */
    def ++(attributes: SchemaAttribute*): SchemaObject = add(schema)("", attributes.toList)

    /**
     * Removes all fields from this schema that are part of the given schema.
     * 
     * @param otherSchema
     *            the schema whose attributes should be removed from this schema
     *
     * @return the updated schema with the attributes of the other schema being removed
     */
    def --(otherSchema: SchemaObject): SchemaObject = extract(schema, otherSchema)

    /**
     * Removes the attribute referenced by the path within this schema.
     *
     * @param path
     *           the path to the attribute to be removed
     *
     * @return the updated schema with the attribute being removed
     */
    def -(path: String): SchemaObject = remove(schema, toSchemaPaths(List(path)))

    /**
     * Removes all attributes that are referenced by the list of paths within this schema.
     *
     * @param paths
     *           the paths to the attributes to be removed
     *
     * @return the updated schema with the attributes being removed
     */
    def --(paths: String*): SchemaObject = remove(schema, toSchemaPaths(paths.toList))

    /**
     * Removes all values that are referenced by the list of paths within this schema.
     *
     * @param paths
     *           the paths to the attributes to be removed
     *
     * @return the updated schema with the attributes being removed
     */
    def --(paths: List[String]): SchemaObject = remove(schema, toSchemaPaths(paths))

    /**
     * Update this schema by predicate.
     *
     * @param predicate
     *            the predicate that needs to evaluate to true in order to execute the update function
     * @param updateFn
     *            the update function
     * @return the updated schema
     */
    def updateByPredicate(predicate: SchemaType => Boolean, updateFn: SchemaType => SchemaType): SchemaObject =
      updateIf(schema)(predicate)(updateFn).asInstanceOf[SchemaObject]


    /**
     * Update this schema by type.
     *
     * @param updateFn
     *           the update function
     * @tparam A
     *           the type that needs to be matched in order for the update function to be executed
     * @return the updated schema
     */
    def updateByType[A <: SchemaType : ClassTag](updateFn: A => SchemaType): SchemaObject = {
      val clazz = implicitly[ClassTag[A]].runtimeClass
      updateIf(schema)(schemaType => clazz.isInstance(schemaType))(updateFn).asInstanceOf[SchemaObject]
    }

    // TODO: caller must cast to object
    /**
     * Updates all attributes for which the given predicate evaluates to true.
     *
     * @param predicate
     *                 the predicate that must be fulfilled in order to update the attribute
     * @param updateFn
     *                 the update function
     * @return the updated schema
     */
    def updateAttributesByPredicate(predicate: SchemaAttribute => Boolean)(updateFn: SchemaAttribute => SchemaAttribute): SchemaType =
      updateAttributeIf(schema)(predicate)(updateFn)

    def update(schemaType: SchemaType, pf: PartialFunction[SchemaType, SchemaType]) =
      self.updateIf(schemaType, pf)

    /**
     * Checks if a given predicate holds for all attributes.
     *
     * @param predicate
     *            the predicate that should be evaluated against all attributes
     * @return true, if the predicate holds for all attributes, false otherwise
     */
    def forAll(predicate: SchemaAttribute => Boolean): Boolean = {
      import scalaz.std.anyVal.booleanInstance.conjunction
      implicit val M = conjunction
      collapse(schema)(predicate)
    }

    /**
     * Compares the schema to another schema.
     *
     * @param otherSchema
     *           the schema to be compared against this one
     * @return true, if the schemas are equal, false otherwise
     */
    def isEquals(otherSchema: SchemaObject): Boolean =
      areEqual(schema, otherSchema)

    /**
     * Checks whether this schema is a subset of the given schema.
     *
     * @param otherSchema
     *           the schema that is supposed to contain this schema as a subset
     * @return true, if this schema is a subset of the given schema
     */
    def isSubSetOf(otherSchema: SchemaObject): Boolean =
      isSubSet(schema, otherSchema)

  }
}
