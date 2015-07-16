package com.eclipsesource.schema

import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{validators, Context, Keywords, Results}
import play.api.data.mapping.{Success, VA}
import play.api.libs.json._

import scala.util.Try


sealed trait SchemaType {
  self: SchemaType =>
//  def validate[T](implicit rds: Reads[T]): JsResult[T] = rds.reads(this)
//  def validate2[S](json: => JsValue, context: Context)(implicit v: Validator3[S]): VA[JsValue] = {
//    v.validate(this, json, context)
//  }

  // http://stackoverflow.com/questions/23481991/attempting-to-model-f-bounded-polymorphism-as-a-type-member-in-scala
//  type Sub >: self.type <: SchemaType
//  def validator: Validator2[Sub]
//  def validate(json: => JsValue, context: Context): VA[JsValue] = {
//    Results.merge(
//      validator.validate(this, json, context),
//      AnyConstraintValidator.validate(json, constraints.any, context)
//    )
//  }
  def constraints: HasAnyConstraint
//  private[schema] def noValidator: Validator2[Sub]= new Validator2[Sub] {
//    override def validate(schema: Sub, json: => JsValue, context: Context): VA[JsValue] = Success(json)
//  }

}

sealed trait PrimitiveSchemaType extends SchemaType

final case class SchemaNull(constraints: NullConstraints) extends SchemaType {
//  type Sub = SchemaNull
//  override def validator: Validator2[SchemaNull] = NullValidator
}



// TODO is the any constraint validator called twice for these types?
final case class SchemaBooleanConstant(bool: Boolean) extends SchemaType {
//  type Sub = SchemaBooleanConstant
  override def constraints: HasAnyConstraint = NoConstraints
//  def validator: Validator2[SchemaBooleanConstant] = noValidator
}
final case class SchemaArrayConstant(seq: Seq[JsValue]) extends SchemaType {
//  type Sub = SchemaArrayConstant
  override def constraints: HasAnyConstraint = NoConstraints
//  override def validator: Validator2[SchemaArrayConstant] = noValidator
}


trait Resolvable {
  def resolvePath(path: String): Option[SchemaType]
}

sealed trait SchemaContainer extends SchemaType with Resolvable {

  def id: Option[String]

  def schemaTypes: Seq[SchemaType]

  def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer
  // TODO: replace string with node trait
}

sealed trait HasProperties extends SchemaType with Resolvable {
  def properties: Seq[SchemaAttribute]
}

final case class JSONPointer(path: String)

// TODO: pointer is a JSONSPointer, see http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-02
// TODO isRemote also applies for non http
final case class SchemaRef(pointer: JSONPointer, isAttribute: Boolean = false, isRemote: Boolean = false) extends SchemaType {
//  type Sub = SchemaRef
  override def constraints = NoConstraints
//  override def validator: Validator2[SchemaRef] = noValidator
}

final case class CompoundSchemaType(oneOf: Seq[SchemaType]) extends SchemaType {
//  type Sub = CompoundSchemaType
//  TODO
  override def constraints: HasAnyConstraint = BooleanConstraints()// CompoundConstraints(oneOf.map(s => s.constraints), AnyConstraint())
//  override def validator: Validator2[CompoundSchemaType] = CompoundValidator
}

final case class SchemaObject(properties: Seq[SchemaAttribute] = Seq.empty,
                    constraints: ObjectConstraints = ObjectConstraints(),
                    id: Option[String] = None)
  extends HasProperties {

//  type Sub = SchemaObject
  override def resolvePath(attributeName: String): Option[SchemaType] = attributeName match {
    case Keywords.Object.Properties => Some(SchemaObject(properties))
    case other => properties.find(_.name == other).map(_.schemaType).fold(
      constraints.resolvePath(attributeName)
    )(Some(_))
  }

//  override val validator: Validator2[SchemaObject] = ObjectValidator
}

final case class SchemaTuple(items: () => Seq[SchemaType],
                       size: Int,
                       constraints: ArrayConstraints = ArrayConstraints(),
                       id: Option[String] = None)
  extends SchemaContainer {

//  type Sub = SchemaTuple
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

//  override def validator: Validator2[SchemaTuple] = TupleValidator
}

final case class SchemaArray(schemaType: () => SchemaType,
                       constraints: ArrayConstraints = ArrayConstraints(),
                       id: Option[String] = None)
  extends SchemaContainer {

//  type Sub = SchemaArray
  lazy val items = schemaType()

  def resolvePath(path: String): Option[SchemaType] = {
    if (path == "items") {
      Some(items)
    } else {
      None
    }
  }

  override def schemaTypes: Seq[SchemaType] = Seq(items)

  override def updated(id: Option[String], containedTypes: SchemaType*): SchemaContainer = copy(schemaType = () => containedTypes.head, id = id)

//  override def validator: Validator2[SchemaArray] = ArrayValidator
}

final case class SchemaString(constraints: StringConstraints = StringConstraints()) extends PrimitiveSchemaType {
//  type Sub = SchemaString
//  override def validator: Validator2[SchemaString] = StringValidator
}

final case class SchemaNumber(constraints: NumberConstraints = NumberConstraints()) extends PrimitiveSchemaType {
//  type Sub = SchemaNumber
//  override def validator: Validator2[SchemaNumber] = NumberValidator
}

final case class SchemaInteger(constraints: NumberConstraints = NumberConstraints()) extends PrimitiveSchemaType {
//  type Sub = SchemaInteger
//  override def validator: Validator2[SchemaInteger] = IntegerValidator
}

final case class SchemaBoolean(constraints: BooleanConstraints = BooleanConstraints()) extends PrimitiveSchemaType {
//  type Sub = SchemaBoolean
//  override def validator: Validator2[SchemaBoolean] = noValidator
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
