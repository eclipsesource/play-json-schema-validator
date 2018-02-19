package com.eclipsesource.schema.internal.constraints

import com.eclipsesource.schema.SchemaType
import play.api.libs.json._

import scalaz.std.option._
import scalaz.std.set._
import scalaz.syntax.semigroup._

trait IsResolvable[A] {
  def resolvePath(constraint: A, path: String): Option[SchemaType]
}

object Constraints {
  import com.eclipsesource.schema._

  trait HasAnyConstraint extends Constraint {
    def any: AnyConstraint
  }

  trait Constraint {
    type A
    def subSchemas: Set[SchemaType]
  }

  case class NoConstraints(any: AnyConstraint = AnyConstraint(None, None, None, None))
    extends HasAnyConstraint {
    type A = NoConstraints
    override def subSchemas: Set[SchemaType] = Set.empty
  }

  case class AnyConstraint(schemaTypeAsString: Option[String] = None,
                           allOf: Option[Seq[SchemaType]] = None,
                           anyOf: Option[Seq[SchemaType]] = None,
                           oneOf: Option[Seq[SchemaType]] = None,
                           definitions: Option[Map[String, SchemaType]] = None,
                           enum: Option[Seq[JsValue]] = None,
                           const: Option[JsValue] = None,
                           not: Option[SchemaType] = None,
                           description: Option[String] = None,
                           id: Option[String] = None,
                           _if: Option[SchemaType] = None,
                           _then: Option[SchemaType] = None,
                           _else: Option[SchemaType] = None
                          )
    extends Constraint {

    type A = AnyConstraint

    def typeGiven = schemaTypeAsString.isDefined

    override def subSchemas: Set[SchemaType] =
      (definitions.map(_.values.toSet) |+|  allOf.map(_.toSet) |+| anyOf.map(_.toSet) |+| oneOf.map(_.toSet))
        .getOrElse(Set.empty[SchemaType])
  }

  case class ObjectConstraints(additionalProps: Option[SchemaType] = None,
                               dependencies: Option[Map[String, SchemaType]] = None,
                               patternProps: Option[Map[String, SchemaType]] = None,
                               required: Option[Seq[String]] = None,
                               minProperties: Option[Int] = None,
                               maxProperties: Option[Int] = None,
                               propertyNames: Option[SchemaType] = None,
                               any: AnyConstraint = AnyConstraint(None, None, None, None))
    extends HasAnyConstraint {

    type A = ObjectConstraints

    def additionalPropertiesOrDefault: SchemaType =
      additionalProps.fold(ObjectConstraints.emptyObject)(identity)

    override def subSchemas: Set[SchemaType] =
      (additionalProps.map(Set(_)) |+|  dependencies.map(_.values.toSet) |+| patternProps.map(_.values.toSet))
        .getOrElse(Set.empty[SchemaType]) ++ any.subSchemas
  }

  object ObjectConstraints {
    def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())
  }

  case class ArrayConstraints(maxItems: Option[Int] = None,
                              minItems: Option[Int] = None,
                              additionalItems: Option[SchemaType] = None,
                              unique: Option[Boolean] = None,
                              contains: Option[SchemaType] = None,
                              any: AnyConstraint = AnyConstraint()) extends HasAnyConstraint {

    type A = ArrayConstraints

    override def subSchemas: Set[SchemaType] =
      (additionalItems.map(Set(_)) |+|  contains.map(Set(_))).getOrElse(Set.empty) ++ any.subSchemas
  }

  case class Minimum(min: BigDecimal, isExclusive: Option[Boolean])
  case class Maximum(max: BigDecimal, isExclusive: Option[Boolean])
  case class MultipleOf(factor: BigDecimal)


  case class NumberConstraints(min: Option[Minimum] = None,
                               max: Option[Maximum] = None,
                               multipleOf: Option[BigDecimal] = None,
                               format:  Option[String] = None,
                               any: AnyConstraint = AnyConstraint())
    extends HasAnyConstraint {

    type A = NumberConstraints

    override def subSchemas: Set[SchemaType] = any.subSchemas
  }

  case class StringConstraints(minLength: Option[Int] = None,
                               maxLength: Option[Int] = None,
                               pattern: Option[String] = None,
                               format:  Option[String] = None,
                               any: AnyConstraint = AnyConstraint())
    extends HasAnyConstraint {

    type A = StringConstraints

    override def subSchemas: Set[SchemaType] = any.subSchemas
  }
}
