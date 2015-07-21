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


  def resolveAttribute(path: SchemaPath, resolvable: SchemaObject): Option[SchemaAttribute] = {

    @tailrec
    def _resolve(path: SchemaPath, attribute: Option[SchemaAttribute], resolvable: SchemaObject): Option[SchemaAttribute] = {
      path match {
        case Nil =>
          attribute
        case pathHead :: pathTail if pathHead.isEmpty => // safety check for root paths
          attribute
        case pathHead :: pathTail =>
          if (pathHead.isEmpty) {
            attribute
          } else {
            val attr = resolvable.properties.find(_.name == pathHead).getOrElse(fail("field.does.not.exist [" + pathHead + "]"))
            attr.schemaType match {
              case cls: SchemaObject => _resolve(pathTail, Some(attr), cls)
              case otherType => Some(attr)
            }
          }
      }
    }

    _resolve(path, None, resolvable)
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
   * Traverses the given type and executes the given modifier if the predicate evaluates to true.
   *
   * @param schemaType
   *               the type to be traversed
   * @param predicate
   *               the predicate that must be fulfilled in order to execute the update function
   * @param modifier
   *               the update function that is called if the predicate is fulfilled
   * @return the updated schema  type
   */
  def updateIf[A <: SchemaType](schemaType: SchemaType)(predicate: SchemaType => Boolean)(modifier: A => SchemaType): SchemaType = {
    schemaType match {
      case cls: SchemaObject =>
        val updatedSchemaObject = cls.copy(properties = cls.properties.map { attr =>
          SchemaAttribute(attr.name, updateIf[A](attr.schemaType)(predicate)(modifier))
        })
        if (predicate(cls)) {
          modifier(updatedSchemaObject.asInstanceOf[A])
        } else {
          updatedSchemaObject
        }
      case arr: SchemaArray => SchemaArray(() => updateIf[A](arr.items)(predicate)(modifier))
      case q if predicate(q) => modifier(q.asInstanceOf[A])
      case _ => schemaType
    }
  }

  /**
   * Traverses the given schema type and executes the given partial function, if matched.
   *
   * @param schemaType
   *               the type to be traversed
   * @param pf
   *               the partial function to be executed
   * @return the updated type
   */
  def updateIf(schemaType: SchemaType, pf: PartialFunction[SchemaType, SchemaType]): SchemaType =
    updateIf(schemaType)(pf.isDefinedAt)(pf.apply)

  /**
   * Traverses the given schema type and executes the given attribute modifier if the predicate evaluates to true.
   *
   * @param schemaType
   *               the type to be traversed
   * @param predicate
   *               the predicate that must be fulfilled in order to execute the update function
   * @param modifier
   *               the attribute update function that is called if the predicate is fulfilled
   * @return the updated schema type
   */
  def updateAttributeIf(schemaType: SchemaObject)(predicate: SchemaAttribute => Boolean)(modifier: SchemaAttribute => SchemaAttribute): SchemaObject = {
    updateIf(schemaType, {
      case obj: SchemaObject =>
        obj.copy(properties = obj.properties.collect {
          case attr if predicate(attr) =>
            val modifiedAttribute = modifier(attr)
            modifiedAttribute.copy(schemaType = modifiedAttribute.schemaType)
          case attr if attr.schemaType.isInstanceOf[SchemaObject] =>
            // TODO check parent
            SchemaAttribute(attr.name, updateAttributeIf(attr.schemaType.asInstanceOf[SchemaObject])(predicate)(modifier))
          case attr => attr
        })
    }).asInstanceOf[SchemaObject]
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
   * Traverses the given schema and executes the adapter function in order
   * to derive a JSON value.
   *
   * @param schema
   *             the schema to be traversed
   * @param path
   *             the current path
   * @param adapter
   *             the adapter to function that is used to produce a JSON value
   *
   * @return the derived JSON vlaue
   */
  def adapt(schema: SchemaType, path: JsPath, adapter: (JsPath, SchemaType) => JsResult[JsValue]): JsResult[JsValue] = {
    schema match {
      case obj: SchemaObject =>
        val fields = obj.properties.map(fd => fd.name -> adapt(fd.schemaType, path \ fd.name, adapter))
          JsSuccess(JsObject(fields.collect {
            case (fieldName, JsSuccess(res, _)) if !res.isInstanceOf[JsUndefined] =>
              (fieldName, res)
          }))
      case q => adapter(path, q)
    }
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
   * Folds over the given schema definition, executes the modifier
   * for each encountered attribute, if the given type is encountered
   * and joins the result by means of the monoid append operation in scope.
   *
   * @param matcher
   *               the predicate to be fulfilled in order to execute the modifier
   * @param schema
   *               the schema to be fold over
   * @param modifier
   *               the modifier to be executed for each attribute
   * @tparam B the result type of the modifier. Must be a monoid instance
   * @return the folded result
   */
  def collapseWithPath[B : Monoid](matcher: SchemaType => Boolean)(schema: SchemaObject)(modifier: (SchemaAttribute, JsPath) => B): B = {

    def _collapseWithPath(matcher: SchemaType => Boolean)(obj: SchemaObject, path: JsPath, result: B)(modifier: (SchemaAttribute, JsPath) => B): B = {
      obj.properties.foldLeft(result)((res, attr) => attr.schemaType match {
        case obj: SchemaObject if matcher(obj) =>
          _collapseWithPath(matcher)(obj, path \ attr.name, res |+| modifier(attr, path \ attr.name))(modifier)
        case obj: SchemaObject =>
          _collapseWithPath(matcher)(obj, path, result)(modifier)
        case a if matcher(a) => res |+| modifier(attr, path)
        case a => res
      })
    }

    val m = implicitly[Monoid[B]]
    _collapseWithPath(matcher)(schema, JsPath(), m.zero)(modifier)
  }

  /**
   * Resolves the given path against the given schema object definition.
   *
   * @param root
   *            the schema object definition against which the path will be resolved
   * @param path
   *            the path to be resolved
   * @return the resolved schema type
   *
   * @tparam A the expected type after resolving is finished
   */
  def resolvePath[A <: SchemaType](root: SchemaObject)(path: SchemaPath): A =
    resolve(path, root)._2.asInstanceOf[A]

  /**
   * Retains all attributes of the schema object definition at the given path based
   * on the given sequence of attribute names.
   *
   * @param root
   *            the schema object definition against which the path will be resolved
   * @param path
   *            the path to be resolved
   * @param attributes
   *            the name of the attributes to be retained
   *
   * @return the updated schema object definition
   */
  def retain(root: SchemaObject)(path: SchemaPath, attributes: Seq[String]): SchemaObject =
    updateByPath[SchemaObject](root, path, cls => {
      cls.copy(properties = cls.properties.filter(field => attributes.contains(field.name)))
    })

  /**
   * Renames the attribute located at the given path.
   *
   * @param root
   *            the schema object definition against which the path will be resolved
   * @param path
   *            the path to be resolved
   * @param newAttributeName
   *            the new attribute name
   *
   * @return the schema object definition with the renamed attribute
   */
  // TODO: duplicate check, TEST
  def renameAttribute(root: SchemaObject)(path: SchemaPath, newAttributeName: String): SchemaObject =
    updateAttributeByPath(root)(path, attr => attr.copy(name = newAttributeName))


  /**
   * Returns the path of the given sub schema, if it is contained in the schema.
   *
   * @param schema
   *          the schema that is supposed to contain the sub-schema
   * @param subSchema
   *          the sub-schema that is supposed to be contained in the first schema
   * @return the path of sub-schema in the schema, if it is contained, None otherwise
   */
  def pathOfSubSchema(schema: SchemaObject, subSchema: SchemaObject): Option[String] = {
    collapseWithPath(_ => true)(schema)((attr, path) => if (attr.schemaType == subSchema) Some(path.toString()) else None)
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
   * Removes all values that are referenced by the list of paths within the given object.
   *
   * @param schema
   *            the schema from which to remove attributes
   * @param paths
   *            the paths to the attributes that are to be removed
   */
  def remove(schema: SchemaObject, paths: Seq[SchemaPath]): SchemaObject =
    paths.foldLeft(schema)((obj, path) => {
      val objPath = if (path.size > 1) path.init else List("")
      val attributeName = if (path.size == 1) path.head else path.last
      updateByPath[SchemaObject](obj, objPath, cls => {
        cls.copy(properties = cls.properties.filterNot(_ == attribute(cls, attributeName)))
      })
    })

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
   * Removes all attributes from the first schema that are also part of the second given schema.
   *
   * @param schemaObj
   *           the schema object definition from which attributes should be removed
   * @param otherObj
   *           the schema object definition that contains all attributes that should be removed from the first one
   *
   * @return the schema object definition without any attributes that are contained in the second
   */
  def extract(schemaObj: SchemaObject, otherObj: SchemaObject) = remove(schemaObj, otherObj.properties.map(field => string2SchemaPath(field.name)))

  /**
   * Compares the schemas with each other.
   *
   * @param schema
   *               the schema to be compared
   * @param otherSchema
     *             the schema to be compared against the first one
   * @return true, if the schemas are equal, false otherwise
   */
  def areEqual(schema: SchemaObject, otherSchema: SchemaObject): Boolean =
    schema.equals(otherSchema)

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
      collapse[SchemaObject, Boolean](schema)(_.schemaType.equals(subSchema))
  }
}
