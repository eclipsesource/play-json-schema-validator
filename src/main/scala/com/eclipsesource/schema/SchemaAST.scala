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
  def updated(fn: SchemaType => SchemaType): SchemaType

}

sealed trait HasId {
  def id: Option[String]
}

//sealed trait PrimitiveSchemaType extends SchemaType

final case class SchemaValue(value: JsValue) extends SchemaType with Resolvable {
  override def constraints: HasAnyConstraint = NoConstraints()
  override def updated(fn: (SchemaType) => SchemaType): SchemaType = this

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

/////////////////

sealed trait SchemaArrayLike extends SchemaType with HasId with Resolvable {
  //  def schemaTypes: Seq[SchemaType]
  def items: Seq[SchemaType]
}

sealed trait SchemaObjectLike extends SchemaType with Resolvable {
  def properties: Seq[Property]
}

/////////////////

final case class JSONPointer(path: String)

final case class CompoundSchemaType(alternatives: Seq[SchemaType]) extends SchemaType {
  override def toString: String = alternatives.map(_.toString).mkString(" ")
  //  TODO: BooleanConstraints is just a placeholder, how to actually handle this case?
  override def constraints: HasAnyConstraint = NoConstraints()// CompoundConstraints(oneOf.map(s => s.constraints), AnyConstraint())
  override def updated(fn: (SchemaType) => SchemaType): SchemaType = this
}

final case class SchemaObject(properties: Seq[Property] = Seq.empty,
                              constraints: ObjectConstraints = ObjectConstraints(),
                              id: Option[String] = None)
  extends HasId with SchemaObjectLike {

  override def toString: String = "object"

  override def resolvePath(attributeName: String): Option[SchemaType] = attributeName match {
    case Keywords.Object.Properties => Some(SchemaObject(properties))
    case other => properties.find(_.name == other).map(_.schemaType).fold(
      constraints.resolvePath(attributeName)
    )(Some(_))
  }

  override def updated(fn: (SchemaType) => SchemaType): SchemaObjectLike =
    copy(
      properties = properties.map(prop => prop.updated(fn)),
      constraints = constraints.updated(fn)
    )
}

final case class SchemaTuple(items: Seq[SchemaType],
                             constraints: ArrayConstraints = ArrayConstraints(),
                             id: Option[String] = None)
  extends SchemaArrayLike {

  override def toString: String = "tuple"

  override def resolvePath(index: String): Option[SchemaType] = {
    index match {
      case Keywords.Array.Items => Some(this)
      case n =>
        val idx = Try { index.toInt }
        idx.toOption.map(i => items(i))
    }
  }

  override def updated(fn: (SchemaType) => SchemaType): SchemaArrayLike =
    copy(items = items.map(fn))
}

final case class SchemaArray(item:  SchemaType,
                             constraints: ArrayConstraints = ArrayConstraints(),
                             id: Option[String] = None)
  extends SchemaArrayLike {

  override def toString: String = "array"

  def items = Seq(item)

  def resolvePath(path: String): Option[SchemaType] = {
    if (path == Keywords.Array.Items) {
      Some(item)
    } else {
      None
    }
  }

  override def updated(fn: (SchemaType) => SchemaType): SchemaArrayLike = {
    copy(item = fn(item))
  }
}

final case class SchemaString(constraints: StringConstraints = StringConstraints()) extends SchemaType {
  override def toString: String = "string"
  override def updated(fn: (SchemaType) => SchemaType): SchemaString = copy(constraints.updated(fn))
}

final case class SchemaNumber(constraints: NumberConstraints = NumberConstraints()) extends SchemaType  {
  override def toString: String = "number"
  override def updated(fn: (SchemaType) => SchemaType): SchemaNumber = copy(constraints.updated(fn))
}

final case class SchemaInteger(constraints: NumberConstraints = NumberConstraints()) extends SchemaType  {
  override def toString: String = "integer"
  override def updated(fn: (SchemaType) => SchemaType): SchemaInteger = copy(constraints.updated(fn))
}

final case class SchemaBoolean(constraints: HasAnyConstraint = NoConstraints()) extends SchemaType{
  override def toString: String = "boolean"
  override def updated(fn: (SchemaType) => SchemaType): SchemaBoolean = copy(constraints = NoConstraints(constraints.any.updated(fn)))
}

final case class SchemaNull(constraints: HasAnyConstraint = NoConstraints()) extends SchemaType  {
  override def toString: String = "null"
  override def updated(fn: (SchemaType) => SchemaType): SchemaNull = copy(constraints = NoConstraints(constraints.any.updated(fn)))
}

sealed trait Property {
  def name: String
  def schemaType: SchemaType
  def updated(fn: SchemaType => SchemaType): Property
}
case class SchemaAttribute(name: String, schemaType: SchemaType) extends Property {
  override def updated(fn: (SchemaType) => SchemaType): Property = copy(schemaType = fn(schemaType))
}


// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
final case class RefAttribute(pointer: String, isRemote: Boolean = false) extends Property {
  override def updated(fn: (SchemaType) => SchemaType) = this
  override def name: String = "$ref"
  override def schemaType: SchemaType = SchemaValue(JsString(pointer))
}


