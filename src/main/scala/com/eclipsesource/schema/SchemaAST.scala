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

sealed trait HasId {
  def id: Option[String]
}

sealed trait PrimitiveSchemaType extends SchemaType with Resolvable {
  def resolvePath(path: String): Option[SchemaType] = constraints.resolvePath(path)
}

final case class SchemaValue(value: JsValue) extends SchemaType with Resolvable {
  override def constraints: HasAnyConstraint = NoConstraints()

  override def resolvePath(path: String): Option[SchemaType] = (value, path) match {
    case (arr: JsArray, index) if Try { index.toInt }.isSuccess =>
      val idx = index.toInt
      if (idx > 0 && idx < arr.value.size){
        Some(SchemaValue(arr.value(idx)))
      } else {
        None
      }
    case other => None
  }
}

sealed trait SchemaArrayLike extends SchemaType with HasId with Resolvable {
  //  def schemaTypes: Seq[SchemaType]
  def items: Seq[SchemaType]
}

sealed trait SchemaObjectLike extends SchemaType with Resolvable {
  def properties: Seq[Property]
}

final case class JSONPointer(path: String)

final case class CompoundSchemaType(alternatives: Seq[SchemaType]) extends SchemaType {
  override def toString: String = alternatives.map(_.toString).mkString(" ")
  override def constraints: HasAnyConstraint = NoConstraints()// CompoundConstraints(oneOf.map(s => s.constraints), AnyConstraint())
}

final case class SchemaObject(properties: Seq[Property] = Seq.empty,
                              constraints: ObjectConstraints = ObjectConstraints(),
                              id: Option[String] = None)
  extends HasId with SchemaObjectLike {

  override def toString: String = "object"

  override def resolvePath(attributeName: String): Option[SchemaType] = {
    attributeName match {
      case Keywords.Object.Properties => Some(this)
      case other => properties.find(_.name == other).map(_.schemaType).fold(
        constraints.resolvePath(attributeName)
      )(Some(_))
    }
  }
}

final case class SchemaTuple(items: Seq[SchemaType],
                             constraints: ArrayConstraints = ArrayConstraints(),
                             id: Option[String] = None)
  extends SchemaArrayLike {

  override def toString: String = "tuple"

  override def resolvePath(index: String): Option[SchemaType] = {

    def isValidIndex(idx: String) = {
      Try {
        val n = idx.toInt
        n <= items.size && n >= 0
      }.toOption.getOrElse(false)
    }

    index match {
      case Keywords.Array.Items => Some(this)
      case idx if isValidIndex(idx) => Some(items(idx.toInt))
      case other => constraints.resolvePath(other)
    }
  }
}

final case class SchemaArray(item:  SchemaType,
                             constraints: ArrayConstraints = ArrayConstraints(),
                             id: Option[String] = None)
  extends SchemaArrayLike {

  override def toString: String = "array"

  def items = Seq(item)

  def resolvePath(path: String): Option[SchemaType] = path match {
    case Keywords.Array.Items => Some(item)
    case other => constraints.resolvePath(other)
  }
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

sealed trait Property {
  def name: String
  def schemaType: SchemaType
}
case class SchemaAttribute(name: String, schemaType: SchemaType) extends Property


// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
final case class RefAttribute(pointer: String, isRemote: Boolean = false) extends Property {
  override def name: String = "$ref"
  override def schemaType: SchemaType = SchemaValue(JsString(pointer))
}


