package com.eclipsesource.schema

import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.json._

sealed trait SchemaType {
  def constraints: Constraint
}

sealed trait HasProps[A] {
  def withProps(props: Seq[(String, SchemaType)]): A
}

sealed trait PrimitiveSchemaType extends SchemaType

final case class SchemaValue(value: JsValue) extends SchemaType {
  override def constraints: Constraint = NoConstraints()
}

sealed trait SchemaArrayLike extends SchemaType {
  def items: Seq[SchemaType]
}

sealed trait SchemaObjectLike extends SchemaType {
  def properties: Seq[SchemaProp]
}

final case class CompoundSchemaType(alternatives: Seq[SchemaType]) extends SchemaType {
  override def toString: String = alternatives.map(_.toString).mkString(" ")
  override def constraints: Constraint = NoConstraints()
}

final case class SchemaMap(name: String, members: Seq[SchemaProp]) extends SchemaType {
  override def constraints: Constraint = NoConstraints()
}

final case class SchemaSeq(members: Seq[SchemaType]) extends SchemaType {
  override def constraints: Constraint = NoConstraints()
}

final case class SchemaObject(properties: Seq[SchemaProp] = Seq.empty,
                              constraints: ObjectConstraints,
                              otherProps: Seq[(String, SchemaType)] = Seq.empty
                             ) extends SchemaObjectLike with HasProps[SchemaObject] {
  override def toString: String = "object"
  override def withProps(otherProps: Seq[(String, SchemaType)]): SchemaObject = copy(otherProps = otherProps)
}

final case class SchemaTuple(items: Seq[SchemaType],
                             constraints: ArrayConstraints,
                             otherProps: Seq[(String, SchemaType)] = Seq.empty) extends SchemaArrayLike with HasProps[SchemaTuple] {
  override def toString: String = "tuple"
  override def withProps(otherProps: Seq[(String, SchemaType)]): SchemaTuple = copy(otherProps = otherProps)
}

// TODO: currently not in use
final case class SchemaRef(ref: String, constraints: HasAnyConstraint) extends SchemaType

final case class SchemaArray(item:  SchemaType,
                             constraints: ArrayConstraints,
                             otherProps: Seq[(String, SchemaType)] = Seq.empty) extends SchemaArrayLike with HasProps[SchemaArray] {

  override def toString: String = "array"
  def items = Seq(item)
  override def withProps(otherProps: Seq[(String, SchemaType)]): SchemaArray = copy(otherProps = otherProps)
}

final case class SchemaString(constraints: StringConstraints) extends PrimitiveSchemaType {
  override def toString: String = "string"
}

final case class SchemaNumber(constraints: NumberConstraints) extends PrimitiveSchemaType {
  override def toString: String = "number"
}

final case class SchemaInteger(constraints: NumberConstraints) extends PrimitiveSchemaType {
  override def toString: String = "integer"
}

final case class SchemaBoolean(constraints: AnyConstraints) extends PrimitiveSchemaType {
  override def toString: String = "boolean"
}

final case class SchemaNull(constraints: AnyConstraints) extends PrimitiveSchemaType {
  override def toString: String = "null"
}

case class SchemaProp(name: String, schemaType: SchemaType)


