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
  implicit class QBSchemaOps(schema: QBClass) {

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
    def resolve[A <: QBType](path: QBStringPath): A = resolvePath[A](schema)(path)

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
    def rename(path: QBStringPath, newAttributeName: String): QBClass =
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
    def rename(oldAttributeName: String, newAttributeName: String): QBClass =
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
    def keep(path: QBStringPath, attributeNames: List[String]): QBClass =
      retain(schema)(path, attributeNames)

    /**
     * Synonym for keep(QBStringPath, List[String]).
     *
     * @param path
     *         the path to the attributes to be kept
     * @param attributeNames
     *         the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes at the given path
     */
    def keepAt(path: QBStringPath, attributeNames: String*): QBClass = 
      retain(schema)(path, attributeNames)

    /**
     * Retains all given attributes of this schema.
     *
     * @param attributes
     *         the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes
     */
    def keep(attributes: String*): QBClass = retain(schema)("", attributes)

    /**
     * Retains all fields of this schema
     *
     * @param attributes
     *          the attributes to be kept
     *
     * @return the updated schema containing only the specified attributes
     */
    def keep(attributes: List[String]): QBClass = retain(schema)("", attributes)

    /**
     * Makes all values referenced by the given list of paths
     * optional.
     *
     * @param paths
     *              the paths to be marked as optional
     *
     * @return the updated schema with the specified paths being marked as optional
     */
    def ?(paths: String*): QBClass = makeOptional(schema, paths.toList.map(string2QBPath))

    /**
     * Makes all values referenced by the given list of paths
     * optional.
     *
     * @param subSchema
     *              the sub-schema to be marked as optional
     *
     * @return the updated schema with the sub-schema being marked as optional
     */
    def ?(subSchema: QBClass): QBClass = pathOfSubSchema(schema, subSchema).fold {
      schema
    } { subPath =>
      val optionalSubSchema = subSchema.updateAttributesByPredicate(_ => true)(attr => attr.addAnnotation(QBOptionalAnnotation()))
      updateByPath[QBClass](schema, subPath.toString.substring(1).replace("/", "."), _ => optionalSubSchema)
    }

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
    def +(path: QBStringPath, attr: QBAttribute): QBClass = add(schema)(path, List(attr))

    /**
     * Adds the given attribute to the root of this schema.
     *
     * @param attr
     *             the attribute to be added
     *
     * @return the updated schema containing the additional attribute
     */
    def +(attr: QBAttribute): QBClass = add(schema)("", List(attr))

    /**
     * Merges the attributes of the given schema into this schema.
     *
     * @param otherSchema
     *           the schema to be merged with this schema
     *
     * @return the updated schema containing the additional attributes from the other schema
     */
    def ++(otherSchema: QBClass): QBClass = merge(schema, otherSchema)

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
    def ++(path: String, attributes: QBAttribute*): QBClass = add(schema)(path, attributes.toList)

    /**
     * Adds the given attributes to the root of this schema.
     *
     * @param attributes
     *            the attributes to be added
     *
     * @return the updated schema containing the additional attributes at the root level
     */
    def ++(attributes: QBAttribute*): QBClass = add(schema)("", attributes.toList)

    /**
     * Removes all fields from this schema that are part of the given schema.
     * 
     * @param otherSchema
     *            the schema whose attributes should be removed from this schema
     *
     * @return the updated schema with the attributes of the other schema being removed
     */
    def --(otherSchema: QBClass): QBClass = extract(schema, otherSchema)

    /**
     * Removes the attribute referenced by the path within this schema.
     *
     * @param path
     *           the path to the attribute to be removed
     *
     * @return the updated schema with the attribute being removed
     */
    def -(path: String): QBClass = remove(schema, toQBPaths(List(path)))

    /**
     * Removes all attributes that are referenced by the list of paths within this schema.
     *
     * @param paths
     *           the paths to the attributes to be removed
     *
     * @return the updated schema with the attributes being removed
     */
    def --(paths: String*): QBClass = remove(schema, toQBPaths(paths.toList))

    /**
     * Removes all values that are referenced by the list of paths within this schema.
     *
     * @param paths
     *           the paths to the attributes to be removed
     *
     * @return the updated schema with the attributes being removed
     */
    def --(paths: List[String]): QBClass = remove(schema, toQBPaths(paths))

    /**
     * Makes all values referenced by the given list of paths
     * read-only.
     *
     * @param paths
     *           the paths to the attributes to be marked as read-only
     *
     * @return the updated schema with the given paths being marked as read-only
     */
    def readOnly(paths: String*) = makeReadOnly(schema, toQBPaths(paths.toList))

    /**
     * Marks all attributes of the given sub-schema as read-only.
     *
     * @param subSchema
     *                  the sub-schema whose attributes should be marked as read-only
     *
     * @return the updated schema, if the sub-schema exists, the unchanged schema otherwise
     */
    def readOnly(subSchema: QBClass) = {
      pathOfSubSchema(schema, subSchema).fold {
        schema
      } { subPath =>
        val readOnlySubSchema = subSchema.updateAttributesByPredicate(_ => true)(_.addAnnotation(QBReadOnlyAnnotation()))
        updateByPath[QBClass](schema, subPath.toString.substring(1).replace("/", "."), _ => readOnlySubSchema)
      }
    }

    /**
     * Update this schema by predicate.
     *
     * @param predicate
     *            the predicate that needs to evaluate to true in order to execute the update function
     * @param updateFn
     *            the update function
     * @return the updated schema
     */
    def updateByPredicate(predicate: QBType => Boolean, updateFn: QBType => QBType): QBClass =
      updateIf(schema)(predicate)(updateFn).asInstanceOf[QBClass]


    /**
     * Update this schema by type.
     *
     * @param updateFn
     *           the update function
     * @tparam A
     *           the type that needs to be matched in order for the update function to be executed
     * @return the updated schema
     */
    def updateByType[A <: QBType : ClassTag](updateFn: A => QBType): QBClass = {
      val clazz = implicitly[ClassTag[A]].runtimeClass
      updateIf(schema)(qbType => clazz.isInstance(qbType))(updateFn).asInstanceOf[QBClass]
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
    def updateAttributesByPredicate(predicate: QBAttribute => Boolean)(updateFn: QBAttribute => QBAttribute): QBType =
      updateAttributeIf(schema)(predicate)(updateFn)

    def update(qbType: QBType, pf: PartialFunction[QBType, QBType]) =
      self.updateIf(qbType, pf)

    def transform(obj: JsObject)(transformers: (QBType => Boolean, PartialFunction[JsValue, JsValue])*): JsObject = self.transform(schema, obj)(transformers)

    /**
     * Checks if a given predicate holds for all attributes.
     *
     * @param predicate
     *            the predicate that should be evaluated against all attributes
     * @return true, if the predicate holds for all attributes, false otherwise
     */
    def forAll(predicate: QBAttribute => Boolean): Boolean = {
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
    def isEquals(otherSchema: QBClass): Boolean =
      areEqual(schema, otherSchema)

    /**
     * Checks whether this schema is a subset of the given schema.
     *
     * @param otherSchema
     *           the schema that is supposed to contain this schema as a subset
     * @return true, if this schema is a subset of the given schema
     */
    def isSubSetOf(otherSchema: QBClass): Boolean =
      isSubSet(schema, otherSchema)

    def isOptional(path: QBStringPath): Boolean = {
      resolveAttribute(path, schema).fold(false)(attr =>
        attr.annotations.exists(_.isInstanceOf[QBOptionalAnnotation])
      )
    }

    def expand(): QBClass = {
      expand(Set())
    }

    def expand(visited: Set[QBRef]): QBClass = {
      val orig = schema
      var _visited = visited
      val expanded = updateByType[QBRef](ref => {
        val resolved: Option[QBType] = schema.resolveRef(ref)
//        if (_visited.contains(ref)) {
//          throw new RuntimeException("circular reference detected")
//        } else {
//          _visited = _visited + ref
//        }
        resolved.getOrElse(throw new RuntimeException("Invalid schema !!TODO!! (only applies to local refs!)"))
      })
//      println(orig)
//      println(expanded)
//      if (orig != expanded) {
//        expanded.expand(_visited)
//      } else {
//        expanded
//      }
      expanded
     }
  }
}
