package com.eclipsesource.schema

import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.json._

import scala.util.Try


trait Resolvable {
  def resolvePath(path: String): Option[SchemaType]
}

sealed trait SchemaType {
  def constraints: HasAnyConstraint
}

sealed trait PrimitiveSchemaType extends SchemaType

final case class SchemaNull(constraints: NullConstraints) extends SchemaType {
  override def toString: String = "null"
}

final case class SchemaBooleanConstant(bool: Boolean) extends SchemaType {
  override def toString: String = "boolean value"
  override def constraints: HasAnyConstraint = NoConstraints
}
final case class SchemaArrayConstant(seq: Seq[JsValue]) extends SchemaType {
  override def toString: String = "array"
  override def constraints: HasAnyConstraint = NoConstraints
}

sealed trait SchemaContainer extends SchemaType with Resolvable {
  def id: Option[String]
  def schemaTypes: Seq[SchemaType]
  def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer
}

sealed trait HasProperties extends SchemaType with Resolvable {
  def properties: Seq[SchemaAttribute]
}

final case class JSONPointer(path: String)

// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
final case class SchemaRef(pointer: JSONPointer, isAttribute: Boolean = false, isRemote: Boolean = false) extends SchemaType {
  override def constraints = NoConstraints
}

final case class CompoundSchemaType(alternatives: Seq[SchemaType]) extends SchemaType {
  override def toString: String = alternatives.map(_.toString).mkString(" ")
//  TODO: BooleanConstraints is just a placeholder, how to actually handle this case?
  override def constraints: HasAnyConstraint = BooleanConstraints()// CompoundConstraints(oneOf.map(s => s.constraints), AnyConstraint())
}

final case class SchemaObject(properties: Seq[SchemaAttribute] = Seq.empty,
                    constraints: ObjectConstraints = ObjectConstraints(),
                    id: Option[String] = None)
  extends HasProperties {

  override def toString: String = "object"

  override def resolvePath(attributeName: String): Option[SchemaType] = attributeName match {
    case Keywords.Object.Properties => Some(SchemaObject(properties))
    case other => properties.find(_.name == other).map(_.schemaType).fold(
      constraints.resolvePath(attributeName)
    )(Some(_))
  }
}

final case class SchemaTuple(items: () => Seq[SchemaType],
                       size: Int,
                       constraints: ArrayConstraints = ArrayConstraints(),
                       id: Option[String] = None)
  extends SchemaContainer {

  override def toString: String = "tuple"

  override def resolvePath(index: String): Option[SchemaType] = {
    index match {
      case Keywords.Array.Items => Some(this)
      case n =>
        val idx = Try { index.toInt }
        idx.toOption.map(i => items()(i))
    }
  }

  override def schemaTypes: Seq[SchemaType] = items()

  override def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer =
    copy(items = () => containedTypes, id = id)

}

final case class SchemaArray(schemaType: () => SchemaType,
                       constraints: ArrayConstraints = ArrayConstraints(),
                       id: Option[String] = None)
  extends SchemaContainer {

  override def toString: String = "array"

  lazy val items = schemaType()

  def resolvePath(path: String): Option[SchemaType] = {
    if (path == Keywords.Array.Items) {
      Some(items)
    } else {
      None
    }
  }

  override def schemaTypes: Seq[SchemaType] = Seq(items)
  override def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer =
    copy(schemaType = () => containedTypes.head, id = id)
}

final case class SchemaString(constraints: StringConstraints = StringConstraints()) extends PrimitiveSchemaType {
  override def toString: String = "string"
}

final case class SchemaNumber(constraints: NumberConstraints = NumberConstraints()) extends PrimitiveSchemaType {
  override def toString: String = "number"
}

final case class SchemaInteger(constraints: NumberConstraints = NumberConstraints()) extends PrimitiveSchemaType {
  override def toString: String = "integer"
}

final case class SchemaBoolean(constraints: BooleanConstraints = BooleanConstraints()) extends PrimitiveSchemaType {
  override def toString: String = "boolean"
}

case class SchemaAttribute(name: String, schemaType: SchemaType)
