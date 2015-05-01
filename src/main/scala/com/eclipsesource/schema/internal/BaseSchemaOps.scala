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

  implicit def string2QBPath(str: String): QBStringPath = str.split("\\.").toList.filterNot(_.trim == "")
  def toQBPaths(paths: List[String]) = paths.toList.map(string2QBPath)
  type QBStringPath = List[String]
  def emptyPath: QBStringPath = List.empty

  def fail[A](msg: String) = throw new RuntimeException(msg)

  case class BuildDescription(descriptions: List[(QBClass, String)]) {

    def +(obj: QBClass, field: String): BuildDescription = {
      BuildDescription(obj -> field :: descriptions)
    }

    def build(initialValue: QBType) = {
      if (descriptions.nonEmpty) {
        val (obj, fieldName) = descriptions.head
        // TODO: recheck parent,
        val init = updateAttributeByPath(obj)(fieldName, _ => QBAttribute(fieldName, initialValue))
        descriptions.tail.foldLeft(init)((updated, desc) => {
          updateAttributeByPath(desc._1)(desc._2, _ => QBAttribute(desc._2, updated))
        })
      } else {
        initialValue.asInstanceOf[QBClass]
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
  def attribute(cls: QBClass, attributeName: String): QBAttribute = {
    cls.attributes.find(_.name == attributeName).getOrElse(fail("field.does.not.exist [" + attributeName + "]"))
  }

  /**
   * Resolves the given path starting from the given resolvable.
   *
   * @param path
   *             the path to be resolved
   * @param resolvable
   *             the QB class definition that contains the attribute that is referenced by the path
   */
  def resolve(path: QBStringPath, resolvable: QBClass): (BuildDescription, QBType) = {

    @tailrec
    def _resolve(path: QBStringPath, resolvable: QBClass,
                 buildDescription: BuildDescription): (BuildDescription, QBType) = {
      path match {
        case Nil =>
          (buildDescription, resolvable)
        case pathHead :: pathTail if pathHead.isEmpty => // safety check for root paths
          (buildDescription, resolvable)
        case pathHead :: pathTail =>
          if (pathHead.isEmpty) {
            (buildDescription, resolvable)
          } else {
            val field = resolvable.attributes.find(_.name == pathHead).getOrElse(fail("field.does.not.exist [" + pathHead + "]"))
            field.qbType match {
              case cls: QBClass => _resolve(pathTail, cls, buildDescription + (resolvable, pathHead))
              case otherType => (buildDescription, otherType)
            }
          }
      }
    }

    _resolve(path, resolvable, BuildDescription.emptyDescription)
  }


  def resolveAttribute(path: QBStringPath, resolvable: QBClass): Option[QBAttribute] = {

    @tailrec
    def _resolve(path: QBStringPath, attribute: Option[QBAttribute], resolvable: QBClass): Option[QBAttribute] = {
      path match {
        case Nil =>
          attribute
        case pathHead :: pathTail if pathHead.isEmpty => // safety check for root paths
          attribute
        case pathHead :: pathTail =>
          if (pathHead.isEmpty) {
            attribute
          } else {
            val attr = resolvable.attributes.find(_.name == pathHead).getOrElse(fail("field.does.not.exist [" + pathHead + "]"))
            attr.qbType match {
              case cls: QBClass => _resolve(pathTail, Some(attr), cls)
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
   * Resolves the given path on the given QB class definition, executes the modifier
   * if the path has been resolved and returns the updated QB class definition.
   *
   * @param resolvable
   *             the QB class definition which is supposed to contain the path
   * @param path
   *             the path  within the class definition that needs to be updated
   * @param modifier
   *             the update function
   * @tparam A
   *             the expected type when the path has been resolved
   * @return the updated QB class definition
   */
  def updateByPath[A <: QBType](resolvable: QBClass, path: QBStringPath, modifier: A => QBType): QBClass = {
    val (buildDescription, value) = resolve(path, resolvable)
    val updated = modifier(value.asInstanceOf[A])
    buildDescription.build(updated)
  }

  /**
   * Traverses the given QB type and executes the given modifier if the predicate evaluates to true.
   *
   * @param qbType
   *               the type to be traversed
   * @param predicate
   *               the predicate that must be fulfilled in order to execute the update function
   * @param modifier
   *               the update function that is called if the predicate is fulfilled
   * @return the updated QB type
   */
  def updateIf[A <: QBType](qbType: QBType)(predicate: QBType => Boolean)(modifier: A => QBType): QBType = {
    qbType match {
      case cls: QBClass =>
        val updatedQBClass = cls.copy(attributes = cls.attributes.map { attr =>
          // TODO check parent
          QBAttribute(attr.name, updateIf[A](attr.qbType)(predicate)(modifier))
        })
        if (predicate(cls)) {
          modifier(updatedQBClass.as[A])
        } else {
          updatedQBClass
        }
        // TODO: check parent
      case arr: QBArray => QBArray(() => updateIf[A](arr.items)(predicate)(modifier), None)
      case q if predicate(q) => modifier(q.as[A])
      case _ => qbType
    }
  }

  /**
   * Traverses the given QB type and executes the given partial function, if matched.
   *
   * @param qbType
   *               the type to be traversed
   * @param pf
   *               the partial function to be executed
   * @return the updated type
   */
  def updateIf(qbType: QBType, pf: PartialFunction[QBType, QBType]): QBType =
    updateIf(qbType)(pf.isDefinedAt)(pf.apply)

  /**
   * Traverses the given QB type and executes the given attribute modifier if the predicate evaluates to true.
   *
   * @param qbType
   *               the type to be traversed
   * @param predicate
   *               the predicate that must be fulfilled in order to execute the update function
   * @param modifier
   *               the attribute update function that is called if the predicate is fulfilled
   * @return the updated QB type
   */
  def updateAttributeIf(qbType: QBClass)(predicate: QBAttribute => Boolean)(modifier: QBAttribute => QBAttribute): QBClass = {
    updateIf(qbType, {
      case obj: QBClass =>
        obj.copy(attributes = obj.attributes.collect {
          case attr if predicate(attr) =>
            val modifiedAttribute = modifier(attr)
            modifiedAttribute.copy(qbType = modifiedAttribute.qbType)
          case attr if attr.qbType.isInstanceOf[QBClass] =>
            // TODO check parent
            QBAttribute(attr.name, updateAttributeIf(attr.qbType.as[QBClass])(predicate)(modifier), attr.annotations)
          case attr => attr
        })
    }).as[QBClass]
  }

  /**
   * Resolves the given path on the given QB class definition, executes the modifier
   * if the path has been resolved and returns the updated QB class definition.
   *
   * @param resolvable
   *               the QB class definition which is supposed to contain the path
   * @param path
   *               the path to be resolved
   * @param modifier
   *               the attribute update function that is called if the predicate is fulfilled
   * @return the updated QB type
   */
  def updateAttributeByPath(resolvable: QBClass)(path: QBStringPath, modifier: QBAttribute => QBAttribute): QBClass = {
    // TODO: init & last may both fail with an exception
    val parentPath = path.init
    val attributeName = path.last
    updateByPath[QBClass](resolvable, parentPath, cls => {
      val currentAttribute = attribute(cls, attributeName)
      cls.attributes.indexOf(currentAttribute) match {
        case -1  => fail("field.does.not.exist [" + attributeName + "]")
        case idx => cls.copy(attributes = cls.attributes.updated(idx, modifier(currentAttribute)))
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
  def adapt(schema: QBType, path: JsPath, adapter: (JsPath, QBType) => JsResult[JsValue]): JsResult[JsValue] = {
    schema match {
      case obj: QBClass =>
        val fields = obj.attributes.map(fd => fd.name -> adapt(fd.qbType, path \ fd.name, adapter))
          JsSuccess(JsObject(fields.collect {
            case (fieldName, JsSuccess(res, _)) if !res.isInstanceOf[JsUndefined] =>
              (fieldName, res)
          }))
      case q => adapter(path, q)
    }
  }

  def transform(schema: QBClass, obj: JsObject)(transformers: Seq[(QBType => Boolean, PartialFunction[JsValue, JsValue])]): JsObject = {
    transformers.foldLeft(new JsValueUpdateBuilder(schema))((builder, entry) =>
      builder.byPredicate(entry._1)(entry._2)
    ).go(obj)
  }

  /**
   * Folds over the given QB schema definition, executes the modifier
   * for each encountered attribute, if the given type is encountered
   * and joins the result by means of the monoid append operation in scope.
   *
   * @param schema
   *               the schema to be fold over
   * @param modifier
   *               the modifier to be executed for each attribute
   * @tparam A a QB subtype that must be matched in order to execute the modifier
   * @tparam B the result type of the modifier. Must be a monoid instance
   * @return the folded result
   */
  def collapse[A <: QBType : ClassTag, B : Monoid](schema: QBClass)(modifier: QBAttribute => B): B = {

    def _collapse(obj: QBClass, result: B)(modifier: QBAttribute => B): B = {
      val clazz = implicitly[ClassTag[A]].runtimeClass
      obj.attributes.foldLeft(result)((res, attr) => attr.qbType match {
        case cls: QBClass if clazz.isInstance(cls) =>
          _collapse(cls, res |+| modifier(attr))(modifier)
        case cls: QBClass =>
          _collapse(cls, res)(modifier)
        case a if clazz.isInstance(a) => res |+| modifier(attr)
        case a => res
      })
    }

    val m = implicitly[Monoid[B]]
    _collapse(schema, m.zero)(modifier)
  }

  /**
   * Folds over the given QB schema definition, executes the modifier
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
  def collapseWithPath[B : Monoid](matcher: QBType => Boolean)(schema: QBClass)(modifier: (QBAttribute, JsPath) => B): B = {

    def _collapseWithPath(matcher: QBType => Boolean)(obj: QBClass, path: JsPath, result: B)(modifier: (QBAttribute, JsPath) => B): B = {
      obj.attributes.foldLeft(result)((res, attr) => attr.qbType match {
        case obj: QBClass if matcher(obj) =>
          _collapseWithPath(matcher)(obj, path \ attr.name, res |+| modifier(attr, path \ attr.name))(modifier)
        case obj: QBClass =>
          _collapseWithPath(matcher)(obj, path, result)(modifier)
        case a if matcher(a) => res |+| modifier(attr, path)
        case a => res
      })
    }

    val m = implicitly[Monoid[B]]
    _collapseWithPath(matcher)(schema, JsPath(), m.zero)(modifier)
  }

  /**
   * Resolves the given path against the given QB class definition.
   *
   * @param root
   *            the QB class definition against which the path will be resolved
   * @param path
   *            the path to be resolved
   * @return the resolved QB type
   *
   * @tparam A the expected type after resolving is finished
   */
  def resolvePath[A <: QBType](root: QBClass)(path: QBStringPath): A =
    resolve(path, root)._2.asInstanceOf[A]

  /**
   * Retains all attributes of the QB class definition at the given path based
   * on the given sequence of attribute names.
   *
   * @param root
   *            the QB class definition against which the path will be resolved
   * @param path
   *            the path to be resolved
   * @param attributes
   *            the name of the attributes to be retained
   *
   * @return the updated QB class definition
   */
  def retain(root: QBClass)(path: QBStringPath, attributes: Seq[String]): QBClass =
    updateByPath[QBClass](root, path, cls => {
      cls.copy(attributes = cls.attributes.filter(field => attributes.contains(field.name)))
    })

  /**
   * Renames the attribute located at the given path.
   *
   * @param root
   *            the QB class definition against which the path will be resolved
   * @param path
   *            the path to be resolved
   * @param newAttributeName
   *            the new attribute name
   *
   * @return the QB class definition with the renamed attribute
   */
  // TODO: duplicate check, TEST
  def renameAttribute(root: QBClass)(path: QBStringPath, newAttributeName: String): QBClass =
    updateAttributeByPath(root)(path, attr => attr.copy(name = newAttributeName))

  /**
   * Makes all values referenced by the given list of paths
   * optional.
   *
   * @param cls
   *            the QB class definition which contains attributes that should be marked
   *            as optional
   * @param paths
   *            a list containing all attribute paths that should be marked as optional
   *
   * @return the updated schema with the referenced attributes being marked as optional
   */
  def makeOptional(cls: QBClass, paths: List[QBStringPath]): QBClass =
    paths.foldLeft(cls)((obj, path) =>
      updateAttributeByPath(obj)(path, _.addAnnotation(QBOptionalAnnotation())))

  /**
   * Marks all values referenced by the given list of paths as read-only.
   *
   * @param schema
   *         the schema that is supposed to contain the attributes that are referenced by the given paths
   * @return the updated schema with the referenced attributes being marked as read-only
   */
  def makeReadOnly(schema: QBClass, paths: List[QBStringPath]): QBClass =
    paths.foldLeft(schema)((obj, path) =>
      updateAttributeByPath(obj)(path, _.addAnnotation(QBReadOnlyAnnotation())))

  /**
   * Returns the path of the given sub schema, if it is contained in the schema.
   *
   * @param schema
   *          the schema that is supposed to contain the sub-schema
   * @param subSchema
   *          the sub-schema that is supposed to be contained in the first schema
   * @return the path of sub-schema in the schema, if it is contained, None otherwise
   */
  def pathOfSubSchema(schema: QBClass, subSchema: QBClass): Option[String] = {
    collapseWithPath(_ => true)(schema)((attr, path) => if (attr.qbType == subSchema) Some(path.toString()) else None)
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
  def add(schema: QBClass)(path: QBStringPath, attributes: Seq[QBAttribute]): QBClass = {
    val fieldNames = attributes.map(_.name)
    updateByPath[QBClass](schema, path, cls =>
      cls.copy(attributes = cls.attributes.filterNot(fd => fieldNames.contains(fd.name)) ++ attributes))
  }

  /**
   * Removes all values that are referenced by the list of paths within the given object.
   *
   * @param schema
   *            the schema from which to remove attributes
   * @param paths
   *            the paths to the attributes that are to be removed
   */
  def remove(schema: QBClass, paths: Seq[QBStringPath]): QBClass =
    paths.foldLeft(schema)((obj, path) => {
      val objPath = if (path.size > 1) path.init else List("")
      val attributeName = if (path.size == 1) path.head else path.last
      updateByPath[QBClass](obj, objPath, cls => {
        cls.copy(attributes = cls.attributes.filterNot(_ == attribute(cls, attributeName)))
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
   * @return the merged QB class definition
   */
  // TODO: duplicate check
  def merge(schema: QBClass, otherSchema: QBClass) = add(schema)(emptyPath, otherSchema.attributes)

  /**
   * Removes all attributes from the first schema that are also part of the second given schema.
   *
   * @param cls
   *           the QB class definition from which attributes should be removed
   * @param otherCls
   *           the QB class definition that contains all attributes that should be removed from the first one
   *
   * @return the first QB class definition without any attributes that are contained in the second
   */
  def extract(cls: QBClass, otherCls: QBClass) = remove(cls, otherCls.attributes.map(field => string2QBPath(field.name)))

  /**
   * Compares the schemas with each other.
   *
   * @param schema
   *               the schema to be compared
   * @param otherSchema
     *             the schema to be compared against the first one
   * @return true, if the schemas are equal, false otherwise
   */
  def areEqual(schema: QBClass, otherSchema: QBClass): Boolean =
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
  def isSubSet(subSchema: QBClass, schema: QBClass): Boolean = {
    import scalaz.std.anyVal.booleanInstance.disjunction
    implicit val M = disjunction
    subSchema.equals(schema) ||
      collapse[QBClass, Boolean](schema)(_.qbType.equals(subSchema))
  }
}
