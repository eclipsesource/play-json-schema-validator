package com.eclipsesource.schema.internal.constraints

import play.api.libs.json._


object Constraints {
  import com.eclipsesource.schema._
  import com.eclipsesource.schema.internal.Keywords

  trait HasAnyConstraint extends Constraint{
    def any: AnyConstraint
  }

  trait Constraint extends Resolvable {

  }

  case class NoConstraints(any: AnyConstraint = AnyConstraint(None, None, None, None))
    extends Constraint with HasAnyConstraint {
    override def resolvePath(path: String): Option[SchemaType] = any.resolvePath(path)
  }

  case class AnyConstraint(schemaTypeAsString: Option[String] = None,
                           allOf: Option[Seq[SchemaType]] = None,
                           anyOf: Option[Seq[SchemaType]] = None,
                           oneOf: Option[Seq[SchemaType]] = None,
                           definitions: Option[Map[String, SchemaType]] = None,
                           enum: Option[Seq[JsValue]] = None,
                           not: Option[SchemaType] = None)
    extends Constraint with Resolvable {

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Any.AllOf => allOf.map(types => SchemaTuple(types))
      case Keywords.Any.AnyOf => anyOf.map(types => SchemaTuple(types))
      case Keywords.Any.OneOf => oneOf.map(types => SchemaTuple(types))
      case Keywords.Any.Definitions => definitions.map(entries => SchemaObject(entries.toSeq.map(e => SchemaAttribute(e._1, e._2))))
      case Keywords.Any.Enum => enum.map(e => SchemaValue(JsArray(e)))
      case _ => None
    }
  }

  case class ObjectConstraints(additionalProps: Option[SchemaType] = None,
                               dependencies: Option[Map[String, SchemaType]] = None,
                               patternProps: Option[Map[String, SchemaType]] = None,
                               required: Option[Seq[String]] = None,
                               minProperties: Option[Int] = None,
                               maxProperties: Option[Int] = None,
                               any: AnyConstraint = AnyConstraint(None, None, None, None))
    extends Constraint with HasAnyConstraint with Resolvable {

    def additionalPropertiesOrDefault: SchemaType = {
      additionalProps.fold(ObjectConstraints.emptyObject)(identity)
    }

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Object.Dependencies => dependencies.map(entries => SchemaObject(entries.toSeq.map(e => SchemaAttribute(e._1, e._2))))
      case Keywords.Object.PatternProperties => patternProps.map(patternProps => SchemaObject(patternProps.toSeq.map(e => SchemaAttribute(e._1, e._2))))
      case Keywords.Object.AdditionalProperties => additionalProps
      case other => any.resolvePath(other)
    }
  }

  object ObjectConstraints {
    def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())
  }

  case class ArrayConstraints(maxItems: Option[Int] = None,
                              minItems: Option[Int] = None,
                              additionalItems: Option[SchemaType] = None,
                              unique: Option[Boolean] = None,
                              any: AnyConstraint = AnyConstraint()) extends Constraint with HasAnyConstraint {

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Array.MinItems => minItems.map(min => SchemaValue(JsNumber(min)))
      case Keywords.Array.MaxItems => maxItems.map(max => SchemaValue(JsNumber(max)))
      case Keywords.Array.AdditionalItems => additionalItems
      case Keywords.Array.UniqueItems => unique.map(u => SchemaValue(JsBoolean(u)))
      case other => any.resolvePath(other)
    }
  }

  case class Minimum(min: BigDecimal, isExclusive: Option[Boolean])
  case class Maximum(max: BigDecimal, isExclusive: Option[Boolean])
  case class MultipleOf(factor: BigDecimal)


  case class NumberConstraints(min: Option[Minimum] = None,
                               max: Option[Maximum] = None,
                               multipleOf: Option[BigDecimal] = None,
                               any: AnyConstraint = AnyConstraint())
    extends Constraint with HasAnyConstraint {

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Number.Min => min.map(m => SchemaValue(JsNumber(m.min)))
      case Keywords.Number.Max => max.map(m => SchemaValue(JsNumber(m.max)))
      case Keywords.Number.MultipleOf => multipleOf.map(m => SchemaValue(JsNumber(m)))
      case other => any.resolvePath(other)
    }
  }

  case class StringConstraints( minLength: Option[Int] = None,
                                maxLength: Option[Int] = None,
                                pattern: Option[String] = None,
                                any: AnyConstraint = AnyConstraint())
    extends Constraint with HasAnyConstraint {

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.String.MinLength => minLength.map(min => SchemaValue(JsNumber(min)))
      case Keywords.String.MaxLength => maxLength.map(max => SchemaValue(JsNumber(max)))
      case Keywords.String.Pattern => pattern.map(p => SchemaValue(JsString(p)))
      case other => any.resolvePath(other)
    }
  }
}
