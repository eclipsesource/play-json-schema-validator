package com.eclipsesource.schema

import play.api.data.mapping.Rule
import play.api.libs.json._

/**
 * Marker for all QBValue types.
 */
sealed trait QBType

object QBType {

  implicit class QBTypeOps(schema: QBType) {
    def as[A <: QBType]: A = schema.asInstanceOf[A]
  }

}

/**
 * Marker for all primitive QB types, which are numbers, strings and booleans.
 *
 * @tparam A
 *             the actual primitive type
 */
trait QBPrimitiveType[A <: JsValue] extends QBType with CompositeRule[A]

/**
 * Marker for base types.
 */
sealed trait QBBaseType

trait QBConstrainedClass extends QBType with ValidationRule[JsObject] {
  def possibleSchemas: Seq[QBClass]
}

case class QBOneOf(possibleSchemas: Seq[QBClass]) extends QBConstrainedClass {
  override def rule: Rule[JsValue, JsValue] = QBOneOfRule(possibleSchemas).rule
}

case class QBAllOf(possibleSchemas: Seq[QBClass]) extends QBConstrainedClass {
  override def rule: Rule[JsValue, JsValue] = QBAllOfRule(possibleSchemas).rule
}

case class QBAnyOf(possibleSchemas: Seq[QBClass]) extends QBConstrainedClass {
  override def rule: Rule[JsValue, JsValue] = QBAnyOfRule(possibleSchemas).rule
}

case class JSONPointer(path: String)

// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
abstract class QBRef(val pointer: JSONPointer) extends QBType with QBBaseType {
  def resolve: QBType
}

/**
 * QBObject constructor.
 *
 * @param attributes
 *             the attributes of an object
 * @param rules
 *             optional rules an object must fulfill
 */
case class QBClass(
  attributes: Seq[QBAttribute],
  rules: Set[ValidationRule[JsObject]] = Set.empty,
  definitions: Map[String, QBType] = Map.empty,
  meta: Map[String, String] = Map.empty
) extends QBType with CompositeRule[JsObject] {

    def apply(attributeName: String): QBType = {
      attributes.find(_.name == attributeName).get.qbType
    }
}

/**
 * Companion object for QBObject.
 */
object QBClassImpl {

  /**
   * Creates an QBObject.
   *
   * @param attributes
   *           the attributes making up the object
   * @return the constructed object
   */
  def apply(attributes: => Seq[QBAttribute]) = new QBClass(attributes, Set.empty)

  /**
   * Creates an QBObject.
   *
   * @param attributes
   *           the attributes making up the object
   * @param rules
   *           an optional set of rules that the QBObject being constructed must adhere
   * @return the constructed object
   */
  def apply(attributes: => Seq[QBAttribute], rules: Set[ValidationRule[JsObject]]) =
    new QBClass(attributes, rules)

  def apply(attributes: => Seq[QBAttribute], rules: Set[ValidationRule[JsObject]], meta: Seq[QBAttribute]) =
    new QBClass(attributes, rules)
}

/**
 * Array
 */
trait QBArray extends QBType with QBBaseType with CompositeRule[JsArray] {
  def items: QBType
  override def toString = "array"
}

/**
 * Companion object for QBArray.
 */
class QBArrayImpl(qbType: () => QBType, val rules: Set[ValidationRule[JsArray]] = Set.empty) extends QBArray {
  lazy val items = qbType()
  override def equals(other: Any): Boolean = other match {
    case that: QBArrayImpl => that.items == items
    case _ => false
  }
}

/**
 * Companion object for QBArray.
 */
object QBArrayImpl {

  /**
   * Creates an QBArray.
   *
   * @param qbType
   *           the type that is contained by the array
   * @return the constructed object
   */
  def apply(qbType: => QBType) = new QBArrayImpl(() => qbType, Set.empty)

  /**
   * Creates an QBArray.
   *
   * @param qbType
   *           the type that is contained by the array
   * @param rules
   *           an optional set of rules that the QBArray being constructed must adhere
   * @return the constructed array
   */
  def apply(qbType: => QBType, rules: Set[ValidationRule[JsArray]]) =
    new QBArrayImpl(() => qbType, rules)
}

/**
 * String
 */
trait QBString extends QBPrimitiveType[JsString] with QBBaseType {
  val rules: Set[ValidationRule[JsString]]
  override def toString = "string"
}

case class QBStringImpl(rules: Set[ValidationRule[JsString]] = Set.empty) extends QBString

/**
 * Number
 */
trait QBNumber extends QBPrimitiveType[JsNumber] with QBBaseType  {
  val rules: Set[ValidationRule[JsNumber]]
  override def toString = "number"
}
case class QBNumberImpl(rules: Set[ValidationRule[JsNumber]] = Set.empty) extends QBNumber

/**
 * Integer
 */
trait QBInteger extends QBPrimitiveType[JsNumber] with QBBaseType  {
  val rules: Set[ValidationRule[JsNumber]]
  override def toString = "integer"
}
case class QBIntegerImpl(rules: Set[ValidationRule[JsNumber]] = Set.empty) extends QBInteger

/**
 * Boolean
 */
trait QBBoolean extends QBPrimitiveType[JsBoolean] with QBBaseType {
  val rules: Set[ValidationRule[JsBoolean]]
  override def toString = "boolean"
}
case class QBBooleanImpl(rules: Set[ValidationRule[JsBoolean]] = Set.empty) extends QBBoolean

/**
 * ----------------------------------------------------------
 * 	Annotations
 * ----------------------------------------------------------
 */
trait QBAnnotation
case class QBAttribute(name: String, qbType: QBType, annotations: Seq[QBAnnotation] = Seq.empty) {
  def addAnnotation(annotation: QBAnnotation): QBAttribute =
    QBAttribute(name, qbType, annotation +: annotations)
}

case class QBDefaultAnnotation(value: JsValue) extends QBAnnotation
case class QBOptionalAnnotation(fallBack: Option[JsValue] = None) extends QBAnnotation
case class QBReadOnlyAnnotation() extends QBAnnotation
case class QBIdAnnotation() extends QBAnnotation
case class QBForeignKeyAnnotation() extends QBAnnotation

/**
 * DSL helper class
 */
case class AnnotatedQBType(qbType: QBType, annotations: List[QBAnnotation]) extends QBType

/**
 * ----------------------------------------------------------
 * 	Custom date types
 * ----------------------------------------------------------
 */
trait QBDateTime extends QBType {
  override def toString = "dateTime"
}
class QBDateTimeImpl(rules: Set[ValidationRule[JsString]]) extends QBStringImpl(rules) with QBDateTime

trait QBPosixTime extends QBType {
  override def toString = "posixTime"
}
class QBPosixTimeImpl(rules: Set[ValidationRule[JsNumber]]) extends QBNumberImpl(rules) with QBPosixTime

