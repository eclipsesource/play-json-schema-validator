package com.eclipsesource.schema.internal

import com.eclipsesource.schema._
import play.api.libs.json._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scalaz.Monoid
import scalaz.syntax.monoid._
import scalaz.std.string._
import scalaz.std.option._

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
   * Folds over the given schema definition, executes the modifier
   * for each encountered attribute, if the given type is encountered
   * and joins the result by means of the monoid append operation in scope.
   *
   * @param schema
   *               the schema to be fold over
   * @param modifier
   *               the modifier to be executed for each attribute
   * @tparam A a schema subtype that must be matched in order to execute the modifier
   * @tparam B the result type of the modifier. Must be a monoid instance
   * @return the folded result
   */
  def collapse[A <: SchemaType : ClassTag, B : Monoid](schema: SchemaObject)(modifier: SchemaAttribute => B): B = {

    def _collapse(obj: SchemaObject, result: B)(modifier: SchemaAttribute => B): B = {
      val clazz = implicitly[ClassTag[A]].runtimeClass
      obj.properties.foldLeft(result)((res, attr) => attr.schemaType match {
        case cls: SchemaObject if clazz.isInstance(cls) =>
          _collapse(cls, res |+| modifier(attr))(modifier)
        case cls: SchemaObject =>
          _collapse(cls, res)(modifier)
        case a if clazz.isInstance(a) => res |+| modifier(attr)
        case a => res
      })
    }

    val m = implicitly[Monoid[B]]
    _collapse(schema, m.zero)(modifier)
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

  /**
   * Checks whether the first schema is a subset of the second.
   *
   * @param subSchema
   *               the schema that is supposed to be a subset
   * @param schema
   *               the schema to check the sub schema against
   * @return true, if the sub schema is a subset of the schema
   */
  def isSubSet(subSchema: SchemaObject, schema: SchemaObject): Boolean = {
    import scalaz.std.anyVal.booleanInstance.disjunction
    implicit val M = disjunction
    subSchema.equals(schema) ||
      collapse[SchemaObject, Boolean](schema)(_.schemaType == subSchema)
  }
}
