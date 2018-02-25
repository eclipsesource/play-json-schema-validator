package com.eclipsesource.schema.internal.constraints

import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json._
import scalaz.Success

object Constraints {

  import com.eclipsesource.schema._

  trait HasAnyConstraint {
    def any: AnyConstraints
  }

  trait Constraint {
    def subSchemas: Set[SchemaType]

    def resolvePath(path: String): Option[SchemaType]

    def validate(schemaType: SchemaType, json: JsValue, context: SchemaResolutionContext)
                (implicit lang: Lang): VA[JsValue]
  }

  case class NoConstraints() extends Constraint {
    override def subSchemas: Set[SchemaType] = Set.empty
    override def resolvePath(path: String): Option[SchemaType] = None
    override def validate(schema: SchemaType, json: JsValue, context: SchemaResolutionContext)
                         (implicit lang: Lang): VA[JsValue] = Success(json)
  }



  trait AnyConstraints extends Constraint {
    def id: Option[String]
    def schemaTypeAsString: Option[String]
  }

  trait ArrayConstraints extends Constraint with HasAnyConstraint

  trait ObjectConstraints extends Constraint with HasAnyConstraint

  trait NumberConstraints extends Constraint

  trait StringConstraints extends Constraint

  case class Minimum(min: BigDecimal, isExclusive: Option[Boolean])

  case class Maximum(max: BigDecimal, isExclusive: Option[Boolean])

  case class MultipleOf(factor: BigDecimal)
}