package com.eclipsesource.schema

import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{Context, Keywords, Results}
import play.api.data.mapping.{Success, VA}
import play.api.libs.json._

import scala.util.Try


sealed trait SchemaType {
  def validate(json: => JsValue, context: Context): VA[JsValue]
  def constraints: Constraint
}

sealed trait PrimitiveSchemaType extends SchemaType

trait AnySchemaType

case class SchemaNull() extends SchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsNull => Success(json)
    case _ => Results.failure("Expected null")
  }

  override def constraints: Constraint = ???
}

// TODO
case class SchemaBooleanConstant(bool: Boolean) extends SchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = ???

  override def constraints: Constraint = ???
}
case class SchemaArrayConstant(seq: Seq[JsValue]) extends SchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = ???

  override def constraints: Constraint = ???
}
case class SchemaStringConstant(seq: String) extends SchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = ???

  override def constraints: Constraint = ???
}

trait Resolvable {
  def resolvePath(path: String): Option[SchemaType]
}

trait SchemaContainer extends SchemaType with Resolvable {

  def id: Option[String]

  def schemaTypes: Seq[SchemaType]

  def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer
  // TODO: replace string with node trait
}

trait HasProperties extends SchemaType with Resolvable {
  def properties: Seq[SchemaAttribute]
}

case class JSONPointer(path: String)

// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
// TODO isRemote also applies for non http?
case class SchemaRef(pointer: JSONPointer, isAttribute: Boolean = false, isRemote: Boolean = false) extends SchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = ???

  override def constraints: Constraint = ???
}

case class SchemaObject(
                    properties: Seq[SchemaAttribute] = Seq.empty,
                    constraints: ObjectConstraints = ObjectConstraints(),
                    id: Option[String] = None)
  extends AnySchemaType with HasProperties {

  case class AdditionalProperties(schema: SchemaType)

  override def resolvePath(attributeName: String): Option[SchemaType] = attributeName match {
    case Keywords.Object.Properties => Some(SchemaObject(properties))
    case other => properties.find(_.name == other).map(_.schemaType).fold(
      constraints.resolvePath(attributeName)
    )(Some(_))
  }

  override def validate(json: => JsValue, context: Context): VA[JsValue] = ObjectValidator.validateObject(this, json, context)
}

case class SchemaTuple(items: () => Seq[SchemaType], size: Int, constraints: ArrayConstraints = ArrayConstraints(false, None, None, None, None, AnyConstraint()), id: Option[String] = None) extends SchemaContainer {

  override def resolvePath(index: String): Option[SchemaType] = {
    index match {
      case "items" => Some(this)
      case n =>
        val idx = Try { index.toInt }
        idx.toOption.map(i => items()(i))
    }
  }

  override def schemaTypes: Seq[SchemaType] = items()

  override def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer = copy(items = () => containedTypes, id = id)

  override def validate(json: => JsValue, context: Context): VA[JsValue] = ArrayConstraintValidator.validateTuple(this, json, context)
}

case class SchemaArray(schemaType: () => SchemaType, constraints: ArrayConstraints = ArrayConstraints(false, None, None, None, None, AnyConstraint()), id: Option[String] = None)
  extends SchemaContainer {

  lazy val items = schemaType()

  def apply(attr: SchemaType) = {
    SchemaArray(() => attr, constraints, id)
  }

  def resolvePath(path: String): Option[SchemaType] = {
    if (path == "items") {
      Some(items)
    } else {
      None
    }
  }

  override def schemaTypes: Seq[SchemaType] = Seq(items)

  override def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer = copy(schemaType = () => containedTypes.head, id = id)

  override def validate(json: => JsValue, context: Context): VA[JsValue] = ArrayConstraintValidator.validateArray(this, json, context)
}

case class SchemaString(constraints: StringConstraints = StringConstraints(false, None, None, None, AnyConstraint())) extends PrimitiveSchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = StringValidator.validate(this, json, context)
}

/**
 * Number
 */
case class SchemaNumber(constraints: NumberConstraints = NumberConstraints(false, None, None, None, AnyConstraint())) extends PrimitiveSchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = NumberConstraintValidator.validate(json, constraints)
}

/**
 * Integer
 */
case class SchemaInteger(constraints: NumberConstraints = NumberConstraints(false, None, None, None, AnyConstraint())) extends PrimitiveSchemaType {

  def isInt(json: JsValue): VA[JsValue] = json match {
    case JsNumber(number) if number.isValidInt => Success(json)
    case _ => Results.failure("Expected integer")
  }

  override def validate(json: => JsValue, context: Context): VA[JsValue] = {
    Results.merge(
      NumberConstraintValidator.validate(json, constraints),
      isInt(json)
    )
  }

}

case class SchemaBoolean(constraints: BooleanConstraints = BooleanConstraints(false, AnyConstraint())) extends PrimitiveSchemaType {
  override def validate(json: => JsValue, context: Context): VA[JsValue] = AnyConstraintValidator.validate(json, constraints.any, context)
}

///**
// * ----------------------------------------------------------
// * 	Annotations
// * ----------------------------------------------------------
// */
trait SchemaAnnotation
case class SchemaAttribute(name: String, schemaType: SchemaType, annotations: Seq[SchemaAnnotation] = Seq.empty) {
  def addAnnotation(annotation: SchemaAnnotation): SchemaAttribute =
    SchemaAttribute(name, schemaType, annotation +: annotations)
}

case class SchemaDefaultAnnotation(value: JsValue) extends SchemaAnnotation
case class SchemaOptionalAnnotation(fallBack: Option[JsValue] = None) extends SchemaAnnotation
case class SchemaReadOnlyAnnotation() extends SchemaAnnotation
case class SchemaIdAnnotation() extends SchemaAnnotation
case class SchemaForeignKeyAnnotation() extends SchemaAnnotation
