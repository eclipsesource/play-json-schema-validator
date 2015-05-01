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

trait QBContainer extends QBType with Containee {

  def qbTypes: Seq[QBType]
  // TODO: replace string with node trait
  def resolvePath(path: String): Option[QBType]

  private def toSegments(pointer: String): List[String] = {
    println(s"pointer is $pointer")
    pointer.split("/").toList.map(segment => {
      // perform escaping
      val escaped = segment.replace("~1", "/").replace("~0", "~")
      println("escaped segment: " + escaped)
      escaped
    })
  }


  def resolveRef(ref: QBRef): Option[QBType] = {
    val segments = toSegments(ref.pointer.path)
    segments.headOption match {
      case Some("#") => resolveRef(this, segments.tail)
      case _ => resolveRef(this, segments)
    }
  }

  private def resolveRef(current: QBContainer, segments: List[String]): Option[QBType] = {
    segments match {
      case Nil => Some(current)
      case attribute :: Nil =>
        current.resolvePath(attribute)
      case segment :: rem =>
        current.resolvePath(segment).flatMap {
          case obj: QBClass => resolveRef(obj, rem)
          case notFound => None
        }
    }
  }

  private[schema] def updateParent(t: QBType, newParent: => QBContainer): QBType = {
    t match {
      case obj: QBClass => obj.copy(parent = Some(() => newParent))
      case ref: QBRef => ref.copy(parent = Some(() => newParent))
      case x => x
    }
  }

  private[schema] def createAttributes(attributes: Seq[(String, QBType)], newParent: => QBClass): Seq[QBAttribute] = {
    val attrs: Seq[(String, QBType, List[QBAnnotation])] = attributes.map(x => x._2 match {
      case annotated: AnnotatedQBType => (x._1, annotated.qbType, annotated.annotations)
      case t => (x._1, t, List.empty[QBAnnotation])
    })
    createAttributesWithAnnotations(attrs, newParent)
  }

  private[schema] def createAttributesWithAnnotations(attributes: Seq[(String, QBType, List[QBAnnotation])], newParent: => QBClass) = {
    attributes.map(attr =>
      QBAttribute(attr._1,  updateParent(attr._2, newParent), attr._3)
    )
  }
}

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
case class QBRef(pointer: JSONPointer, parent: Option[() => QBContainer]) extends QBType with Containee

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
                    parent: Option[() => QBContainer],
                    rules: Set[ValidationRule[JsObject]] = Set.empty,
                    definitions: Map[String, QBType] = Map.empty,
                    meta: Map[String, String] = Map.empty
                    ) extends QBContainer with CompositeRule[JsObject] {

  def apply(attrs: (String, QBType)*) = {
    lazy val newParent = QBClass(updatedAttributes, parent, rules, definitions, meta)
    lazy val updatedAttributes: Seq[QBAttribute] = createAttributes(attrs, newParent)
    newParent
  }

  def apply(attrs: List[(String, QBType, List[QBAnnotation])]) = {
    lazy val newParent = QBClass(updatedAttributes, parent, rules, definitions, meta)
    lazy val updatedAttributes: Seq[QBAttribute] = createAttributesWithAnnotations(attrs, newParent)
    newParent
  }

  private def findDuplicates[A, B](list: List[B])(criterion: (B) => A): Seq[A] = {
    (list.groupBy(criterion) filter { case (_, l) => l.size > 1 } keys).toSeq
  }

  private def root: QBContainer = {
    var p: QBContainer = this
    while (p.parent.isDefined) {
      p = p.parent.get()
    }
    p
  }

  override def qbTypes: Seq[QBType] = attributes.map(_.qbType)

  override def resolvePath(attributeName: String): Option[QBType] = {
    attributes.find(_.name == attributeName).map(_.qbType)
  }
}



trait QBTuple2 extends QBContainer with CompositeRule[JsArray] {
  def items: (QBType, QBType)
 // override def toString = s"(${items._1}, ${items._2}})"
  def qbTypes = Seq(items._1, items._2)
}

trait QBTuple3 extends QBType with CompositeRule[JsArray] {
  def items: (QBType, QBType)
  override def toString = "array"
}

trait QBTuple4 extends QBType with CompositeRule[JsArray] {
  def items: (QBType, QBType)
  override def toString = "array"
}

case class QBTuple2Impl(t: () => (QBType, QBType), parent: Option[() => QBContainer], rules: Set[ValidationRule[JsArray]] = Set.empty) extends QBTuple2 with CompositeRule[JsArray] {
  def items = t()

  // TODO: replace string with node trait
  override def resolvePath(index: String): Option[QBType] = {
    val idx = index.replace("items/", "").toInt
    if (idx == 0) {
      Some(t()._1)
    } else if (idx == 1) {
      Some(t()._2)
    } else {
      None
    }
  }

  def apply(attr: (QBType, QBType)): QBTuple2Impl = {
    lazy val newParent = QBTuple2Impl(() => updatedItems, parent)
    lazy val updatedItems: (QBType, QBType) = (
      updateParent(attr._1, newParent),
      updateParent(attr._2, newParent)
    )

    newParent
  }
}

// TODO: more tuples...

/**
 * Companion object for QBArray.
 */
case class QBArray(qbType: () => QBType,  parent: Option[() => QBContainer], rules: Set[ValidationRule[JsArray]] = Set.empty) extends QBContainer with CompositeRule[JsArray] {

  lazy val items = qbType()

  def apply(attr: QBType) = {
    lazy val newParent = QBArray(() => updatedItems, parent, rules)
    lazy val updatedItems: QBType = updateParent(attr, newParent)
    newParent
  }

  def resolvePath(path: String): Option[QBType] = {
    if (path == "items") {
      Some(items)
    } else {
      None
    }
  }

  override def qbTypes: Seq[QBType] = Seq(items)
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

trait Containee {
  def parent: Option[() => QBContainer]
}

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

