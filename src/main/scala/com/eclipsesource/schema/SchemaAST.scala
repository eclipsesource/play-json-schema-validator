package com.eclipsesource.schema

import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.json._


trait Resolvable {
  def resolvePath(path: String): Option[SchemaType]
}

sealed trait SchemaType {
  def constraints: HasAnyConstraint
}

sealed trait HasProps[A] {
  def withProps(schemaObject: SchemaObject): A
}

sealed trait HasId {
  def id: Option[String]
}

sealed trait PrimitiveSchemaType extends SchemaType

final case class SchemaValue(value: JsValue) extends SchemaType {
  override def constraints: HasAnyConstraint = NoConstraints()
}

sealed trait SchemaArrayLike extends SchemaType with HasId {
  def items: Seq[SchemaType]
}

sealed trait SchemaObjectLike extends SchemaType {
  def properties: Seq[SchemaAttribute]
}

final case class CompoundSchemaType(alternatives: Seq[SchemaType]) extends SchemaType {
  override def toString: String = alternatives.map(_.toString).mkString(" ")
  override def constraints: HasAnyConstraint = NoConstraints()// CompoundConstraints(oneOf.map(s => s.constraints), AnyConstraint())
}

final case class SchemaObject(properties: Seq[SchemaAttribute] = Seq.empty,
                              constraints: ObjectConstraints = ObjectConstraints(),
                              id: Option[String] = None) extends HasId with SchemaObjectLike with HasProps[SchemaObject] {
  override def toString: String = "object"
  override def withProps(schemaObject: SchemaObject): SchemaObject = (this ++ schemaObject).copy(
    constraints = constraints,
    id = id
  )
}

final case class SchemaTuple(items: Seq[SchemaType],
                             constraints: ArrayConstraints = ArrayConstraints(),
                             id: Option[String] = None,
                             otherProps: Option[SchemaObject] = None) extends SchemaArrayLike with HasProps[SchemaTuple] {
  override def toString: String = "tuple"
  override def withProps(schemaObject: SchemaObject): SchemaTuple = copy(otherProps = Some(schemaObject))
}

final case class SchemaArray(item:  SchemaType,
                             constraints: ArrayConstraints = ArrayConstraints(),
                             id: Option[String] = None,
                             otherProps: Option[SchemaObject] = None) extends SchemaArrayLike with HasProps[SchemaArray] {
  override def toString: String = "array"
  def items = Seq(item)
  override def withProps(schemaObject: SchemaObject): SchemaArray = copy(otherProps = Some(schemaObject))
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

final case class SchemaBoolean(constraints: HasAnyConstraint = NoConstraints()) extends PrimitiveSchemaType {
  override def toString: String = "boolean"
}

final case class SchemaNull(constraints: HasAnyConstraint = NoConstraints()) extends PrimitiveSchemaType {
  override def toString: String = "null"
}

case class SchemaAttribute(name: String, schemaType: SchemaType)


