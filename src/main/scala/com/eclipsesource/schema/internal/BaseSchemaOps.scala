package com.eclipsesource.schema.internal

import scala.annotation.tailrec

import com.eclipsesource.schema._

/**
 * Primitive schema functions.
 */
trait BaseSchemaOps {

  implicit def string2SchemaPath(str: String): SchemaPath = str.split("\\.").toList.filterNot(_.trim == "")
  def toSchemaPaths(paths: List[String]) = paths.toList.map(string2SchemaPath)
  type SchemaPath = List[String]
  def emptyPath: SchemaPath = List.empty

  def fail[A](msg: String) = throw new RuntimeException(msg)

  case class BuildDescription(descriptions: List[(SchemaObject, String)]) {

    def +(obj: SchemaObject, field: String): BuildDescription = {
      BuildDescription(obj -> field :: descriptions)
    }

    def build(initialValue: SchemaType) = {
      if (descriptions.nonEmpty) {
        val (obj, fieldName) = descriptions.head
        val init = updateAttributeByPath(obj)(fieldName, _ => SchemaAttribute(fieldName, initialValue))
        descriptions.tail.foldLeft(init)((updated, desc) => {
          updateAttributeByPath(desc._1)(desc._2, _ => SchemaAttribute(desc._2, updated))
        })
      } else {
        initialValue.asInstanceOf[SchemaObject]
      }
    }
  }

  object BuildDescription {
    def emptyDescription = BuildDescription(List.empty)
  }

  // Core methods --

  /**
   * Returns the attribute with the given attribute name from the given class description.
   *
   * @param cls
   *          the class description
   * @param attributeName
   *          the attribute name
   * @return the attribute that matches the given attribute. If the attribute does not exists
   *         a RuntimeException is thrown
   */
  def attribute(cls: SchemaObject, attributeName: String): SchemaAttribute = {
    cls.properties.find(_.name == attributeName).getOrElse(fail("field.does.not.exist [" + attributeName + "]"))
  }

  /**
   * Resolves the given path starting from the given resolvable.
   *
   * @param path
   *             the path to be resolved
   * @param resolvable
   *             the schema object definition that contains the attribute that is referenced by the path
   */
  def resolve(path: SchemaPath, resolvable: SchemaObject): (BuildDescription, SchemaType) = {

    @tailrec
    def _resolve(path: SchemaPath, resolvable: SchemaObject,
                 buildDescription: BuildDescription): (BuildDescription, SchemaType) = {
      path match {
        case Nil =>
          (buildDescription, resolvable)
        case pathHead :: pathTail if pathHead.isEmpty => // safety check for root paths
          (buildDescription, resolvable)
        case pathHead :: pathTail =>
          if (pathHead.isEmpty) {
            (buildDescription, resolvable)
          } else {
            val field = resolvable.properties.find(_.name == pathHead).getOrElse(fail("field.does.not.exist [" + pathHead + "]"))
            field.schemaType match {
              case cls: SchemaObject => _resolve(pathTail, cls, buildDescription + (resolvable, pathHead))
              case otherType => (buildDescription, otherType)
            }
          }
      }
    }

    _resolve(path, resolvable, BuildDescription.emptyDescription)
  }

  //
  // Extension methods based on update --
  //

  /**
   * Resolves the given path on the given class definition, executes the modifier
   * if the path has been resolved and returns the updated schema object definition.
   *
   * @param resolvable
   *             the schema object definition which is supposed to contain the path
   * @param path
   *             the path  within the class definition that needs to be updated
   * @param modifier
   *             the update function
   * @tparam A
   *             the expected type when the path has been resolved
   * @return the updated schema object definition
   */
  def updateByPath[A <: SchemaType](resolvable: SchemaObject, path: SchemaPath, modifier: A => SchemaType): SchemaObject = {
    val (buildDescription, value) = resolve(path, resolvable)
    val updated = modifier(value.asInstanceOf[A])
    buildDescription.build(updated)
  }


  /**
   * Resolves the given path on the given schema object definition, executes the modifier
   * if the path has been resolved and returns the updated schema object definition.
   *
   * @param resolvable
   *               the schema object definition which is supposed to contain the path
   * @param path
   *               the path to be resolved
   * @param modifier
   *               the attribute update function that is called if the predicate is fulfilled
   * @return the updated schema type
   */
  def updateAttributeByPath(resolvable: SchemaObject)(path: SchemaPath, modifier: SchemaAttribute => SchemaAttribute): SchemaObject = {
    // TODO: init & last may both fail with an exception
    val parentPath = path.init
    val attributeName = path.last
    updateByPath[SchemaObject](resolvable, parentPath, cls => {
      val currentAttribute = attribute(cls, attributeName)
      cls.properties.indexOf(currentAttribute) match {
        case -1  => fail("field.does.not.exist [" + attributeName + "]")
        case idx => cls.copy(properties = cls.properties.updated(idx, modifier(currentAttribute)))
      }
    })
  }

  /**
   * Adds the given fields to the object located at the path of the given object.
   *
   * @param schema
   *           the schema to which the attributes should be added
   * @param path
   *           the path within the given schema at which the attributes should be added
   * @param attributes
   *           the actual attributes to be added
   */
  def add(schema: SchemaObject)(path: SchemaPath, attributes: Seq[SchemaAttribute]): SchemaObject = {
    val fieldNames = attributes.map(_.name)
    updateByPath[SchemaObject](schema, path, cls =>
      cls.copy(properties = cls.properties.filterNot(fd => fieldNames.contains(fd.name)) ++ attributes))
  }

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
  def merge(schema: SchemaObject, otherSchema: SchemaObject) = add(schema)(emptyPath, otherSchema.properties)
}
