package com.eclipsesource.schema

import java.net.{URL, URLDecoder}

import play.api.data.mapping.Rule
import play.api.libs.json._

import scala.io.Source
import scala.util.Try

/**
 * Marker for all QBValue types.
 */
sealed trait QBType

case class QBNull() extends QBType {}

// TODO
case class QBBooleanConstant(bool: Boolean) extends QBType
case class QBArrayConstant(seq: Seq[String]) extends QBType
case class QBStringConstant(seq: String) extends QBType

/**
 * Marker for all primitive QB types, which are numbers, strings and booleans.
 *
 * @tparam A
 *             the actual primitive type
 */
trait QBPrimitiveType[A <: JsValue] extends QBType with CompositeRule

trait Resolvable {
  def resolvePath(path: String): Option[QBType]
}

trait QBContainer extends QBType with Resolvable {

  def id: Option[String]

  def qbTypes: Seq[QBType]

  def updated(id: Option[String], containedTypes: QBType*): QBContainer
  // TODO: replace string with node trait
}

trait QBObject extends QBType with Resolvable {
  def properties: Seq[QBAttribute]
}

/**
 * Marker for base types.
 */
sealed trait QBBaseType

case class JSONPointer(path: String)

// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
// TODO isRemote also applies for non http?
case class QBRef(pointer: JSONPointer, isAttribute: Boolean = false, isRemote: Boolean = false) extends QBType


/**
 * QBObject constructor.
 *
 * @param properties
 *             the attributes of an object
 * @param rules
 *             optional rules an object must fulfill
 */
case class QBClass(
                    properties: Seq[QBAttribute],
                    rules: Set[ValidationRule] = Set.empty,
                    id: Option[String] = None)
  extends QBObject with CompositeRule {

  case class AdditionalProperties(schema: QBType)

  def dependencies: Seq[QBAttribute] = properties.find(_.name == "dependencies").map(_.qbType match {
    case QBClass(props, _, _) => props
    case _ => throw new RuntimeException("patternProperties must be an object")
  }).getOrElse(Seq.empty)

  def additionalProperties: AdditionalProperties = properties.find(_.name == "additionalProperties").map(_.qbType match {
    case t: QBType => AdditionalProperties(t)
  }).getOrElse(AdditionalProperties(QBClass(Seq.empty)))

  def patternProperties: Seq[QBAttribute] = properties.find(_.name == "patternProperties").map(_.qbType match {
    case QBClass(props, _, _) => props
    case _ => throw new RuntimeException("patternProperties must be an object")
  }).getOrElse(Seq.empty)

  def props: Seq[QBAttribute] = properties.find(_.name == "properties").map(_.qbType match {
    case QBClass(props, _, _) => props
    case _ => throw new RuntimeException("properties must be an object")
  }).getOrElse(Seq.empty)

  override def resolvePath(attributeName: String): Option[QBType] = {
    properties.find(_.name == attributeName).map(_.qbType)
  }
}

// TODO: inconsistent with QBClass where each property is just a QBAttribute
// TODO: idRef
case class QBTuple(items: () => Seq[QBType], size: Int, additionalItems: Option[QBType] = None, rules: Set[ValidationRule] = Set.empty, id: Option[String]) extends QBContainer with CompositeRule {

  // TODO: replace string with node trait
  override def resolvePath(index: String): Option[QBType] = {
    index match {
      case "items" => Some(this)
      case n =>
        val idx = Try { index.toInt }
        idx.toOption.map(i => items()(i))
    }
  }

  def apply(types: Seq[QBType]): QBTuple = {
    QBTuple(() => types, size, additionalItems, rules, id)
  }

  override def qbTypes: Seq[QBType] = items()

  override def updated(id: Option[String], containedTypes: QBType*): QBContainer = copy(items = () => containedTypes, id = id)
}

/**
 * Companion object for QBArray.
 */
// TODO: inconsistent with QBClass where each property is just a QBAttribute

case class QBArray(qbType: () => QBType, rules: Set[ValidationRule] = Set.empty, id: Option[String] = None)
  extends QBContainer with CompositeRule {

  lazy val items = qbType()

  def apply(attr: QBType) = {
    QBArray(() => attr, rules, id)
  }

  def resolvePath(path: String): Option[QBType] = {
    if (path == "items") {
      Some(items)
    } else {
      None
    }
  }

  override def qbTypes: Seq[QBType] = Seq(items)

//  override def isEmptySchema: Boolean = items.isEmptySchema
//  override def setResolutionScope(newScope: String): QBType =
//  copy(resolutionScope = Some(newScope),
//    qbType = () => qbType().setResolutionScope(newScope))

  override def updated(id: Option[String], containedTypes: QBType*): QBContainer = copy(qbType = () => containedTypes.head, id = id)
}

/**
 * String
 */
trait QBString extends QBPrimitiveType[JsString] with QBBaseType {
  val rules: Set[ValidationRule]
  override def toString = "string"
}

case class QBStringImpl(rules: Set[ValidationRule] = Set.empty) extends QBString

/**
 * Number
 */
trait QBNumber extends QBPrimitiveType[JsNumber] with QBBaseType  {
  val rules: Set[ValidationRule]
  override def toString = "number"
}
case class QBNumberImpl(rules: Set[ValidationRule] = Set.empty) extends QBNumber

/**
 * Integer
 */
trait QBInteger extends QBPrimitiveType[JsNumber] with QBBaseType  {
  val rules: Set[ValidationRule]
  override def toString = "integer"
}
case class QBIntegerImpl(intRules: Set[ValidationRule] = Set()) extends QBInteger {
  val rules = intRules + IsIntegerRule()
}

/**
 * Boolean
 */
trait QBBoolean extends QBPrimitiveType[JsBoolean] with QBBaseType {
  val rules: Set[ValidationRule]
  override def toString = "boolean"
}
case class QBBooleanImpl(rules: Set[ValidationRule] = Set.empty) extends QBBoolean

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
case class AnnotatedQBType(qbType: QBType, annotations: List[QBAnnotation]) extends QBType {
//  def isEmptySchema: Boolean = qbType.isEmptySchema
}

/**
 * ----------------------------------------------------------
 * 	Custom date types
 * ----------------------------------------------------------
 */
trait QBDateTime extends QBType {
  override def toString = "dateTime"
}
class QBDateTimeImpl(rules: Set[ValidationRule]) extends QBStringImpl(rules) with QBDateTime

trait QBPosixTime extends QBType {
  override def toString = "posixTime"
}
class QBPosixTimeImpl(rules: Set[ValidationRule] = Set.empty) extends QBNumberImpl(rules) with QBPosixTime

