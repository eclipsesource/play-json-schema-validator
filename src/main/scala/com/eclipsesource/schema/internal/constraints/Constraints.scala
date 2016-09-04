package com.eclipsesource.schema.internal.constraints

import com.eclipsesource.schema.internal.refs.Ref
import play.api.libs.json._

object Constraints {
  import com.eclipsesource.schema._
  import com.eclipsesource.schema.internal.Keywords

  trait HasAnyConstraint extends Constraint {
    def any: AnyConstraint
  }

  trait Constraint extends Resolvable {
    type A
    def merge(otherConstraints: Constraint): A
  }

  case class NoConstraints(any: AnyConstraint = AnyConstraint(None, None, None, None))
    extends HasAnyConstraint {
    type A = NoConstraints
    override def resolvePath(path: String): Option[SchemaType] = any.resolvePath(path)
    override def merge(otherConstraints: Constraint): NoConstraints = this
  }

  case class AnyConstraint(schemaTypeAsString: Option[String] = None,
                           allOf: Option[Seq[SchemaType]] = None,
                           anyOf: Option[Seq[SchemaType]] = None,
                           oneOf: Option[Seq[SchemaType]] = None,
                           definitions: Option[Map[String, SchemaType]] = None,
                           enum: Option[Seq[JsValue]] = None,
                           not: Option[SchemaType] = None,
                           description: Option[String] = None,
                           id: Option[String] = None,
                           anchors: Map[Ref, SchemaType] = Map.empty)
    extends Constraint with Resolvable {

    type A = AnyConstraint

    def typeGiven = schemaTypeAsString.isDefined

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Any.Type  => schemaTypeAsString.map(t => SchemaValue(JsString(t)))
      case Keywords.Any.AllOf => allOf.map(types => SchemaTuple(types))
      case Keywords.Any.AnyOf => anyOf.map(types => SchemaTuple(types))
      case Keywords.Any.OneOf => oneOf.map(types => SchemaTuple(types))
      case Keywords.Any.Definitions => definitions.map(entries =>
        SchemaObject(entries.toSeq.map { case (name, schema) => SchemaAttribute(name, schema) }))
      case Keywords.Any.Enum => enum.map(e => SchemaValue(JsArray(e)))
      case Keywords.Any.Not => not
      case Keywords.Any.Id => id.map(i => SchemaValue(JsString(i)))
      case _ => None
    }

    override def merge(otherConstraints: Constraint): AnyConstraint = otherConstraints match {
      case otherAny: AnyConstraint =>
        AnyConstraint(
          schemaTypeAsString orElse otherAny.schemaTypeAsString,
          (allOf ++ otherAny.allOf).reduceOption(_ ++ _),
          (anyOf ++ otherAny.anyOf).reduceOption(_ ++ _),
          (oneOf ++ otherAny.oneOf).reduceOption(_ ++ _),
          (definitions ++ otherAny.definitions).reduceOption(_ ++ _),
          (enum ++ otherAny.enum).reduceOption(_ ++ _),
          // TODO: could be improved by merging both schemas
          not orElse otherAny.not
          // id is not taken care of
        )
      case other => this
    }
  }

  case class ObjectConstraints(additionalProps: Option[SchemaType] = None,
                               dependencies: Option[Map[String, SchemaType]] = None,
                               patternProps: Option[Map[String, SchemaType]] = None,
                               required: Option[Seq[String]] = None,
                               minProperties: Option[Int] = None,
                               maxProperties: Option[Int] = None,
                               any: AnyConstraint = AnyConstraint(None, None, None, None))
    extends HasAnyConstraint with Resolvable {

    type A = ObjectConstraints

    def additionalPropertiesOrDefault: SchemaType =
      additionalProps.fold(ObjectConstraints.emptyObject)(identity)

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Object.AdditionalProperties => additionalProps
      case Keywords.Object.Dependencies => dependencies.map(entries => SchemaObject(entries.toSeq.map(e => SchemaAttribute(e._1, e._2))))
      case Keywords.Object.PatternProperties => patternProps.map(patternProps => SchemaObject(patternProps.toSeq.map(e => SchemaAttribute(e._1, e._2))))
      case Keywords.Object.MinProperties => minProperties.map(min => SchemaValue(JsNumber(min)))
      case Keywords.Object.MaxProperties => maxProperties.map(max => SchemaValue(JsNumber(max)))
      case other => any.resolvePath(other)
    }

    override def merge(constraints: Constraint): ObjectConstraints = constraints match {
      case otherConstraints: ObjectConstraints =>
        ObjectConstraints(
          additionalProps orElse otherConstraints.additionalProps,
          (dependencies ++ otherConstraints.dependencies).reduceOption(_ ++ _),
          (patternProps ++ otherConstraints.patternProps).reduceOption(_ ++ _),
          (required ++ otherConstraints.required).reduceOption(_ ++ _),
          minProperties orElse otherConstraints.minProperties,
          maxProperties orElse otherConstraints.maxProperties,
          any.merge(otherConstraints.any)
        )
      case withAnyConstraints: HasAnyConstraint => copy(any = any.merge(withAnyConstraints.any))
      case other => this
    }
  }

  object ObjectConstraints {
    def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())
  }

  case class ArrayConstraints(maxItems: Option[Int] = None,
                              minItems: Option[Int] = None,
                              additionalItems: Option[SchemaType] = None,
                              unique: Option[Boolean] = None,
                              any: AnyConstraint = AnyConstraint()) extends HasAnyConstraint {

    type A = ArrayConstraints

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Array.MinItems => minItems.map(min => SchemaValue(JsNumber(min)))
      case Keywords.Array.MaxItems => maxItems.map(max => SchemaValue(JsNumber(max)))
      case Keywords.Array.AdditionalItems => additionalItems
      case Keywords.Array.UniqueItems => unique.map(u => SchemaValue(JsBoolean(u)))
      case other => any.resolvePath(other)
    }

    override def merge(otherConstraints: Constraint): ArrayConstraints = otherConstraints match {
      case otherArrayConstraints: ArrayConstraints => ArrayConstraints(
        maxItems orElse otherArrayConstraints.maxItems,
        minItems orElse otherArrayConstraints.minItems,
        additionalItems orElse otherArrayConstraints.additionalItems,
        unique orElse otherArrayConstraints.unique,
        any.merge(otherArrayConstraints.any)
      )
      case withAnyConstraints: HasAnyConstraint => copy(any = any.merge(withAnyConstraints.any))
      case other => this
    }
  }

  case class Minimum(min: BigDecimal, isExclusive: Option[Boolean])
  case class Maximum(max: BigDecimal, isExclusive: Option[Boolean])
  case class MultipleOf(factor: BigDecimal)


  case class NumberConstraints(min: Option[Minimum] = None,
                               max: Option[Maximum] = None,
                               multipleOf: Option[BigDecimal] = None,
                               any: AnyConstraint = AnyConstraint())
    extends HasAnyConstraint {

    type A = NumberConstraints

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Number.Min => min.map(m => SchemaValue(JsNumber(m.min)))
      case Keywords.Number.Max => max.map(m => SchemaValue(JsNumber(m.max)))
      case Keywords.Number.MultipleOf => multipleOf.map(m => SchemaValue(JsNumber(m)))
      case other => any.resolvePath(other)
    }

    override def merge(otherConstraints: Constraint): NumberConstraints = otherConstraints match {
      case otherConstraints: NumberConstraints =>
        NumberConstraints(
          min orElse otherConstraints.min,
          max orElse otherConstraints.max,
          // TODO: we could look for the gcd here
          multipleOf orElse otherConstraints.multipleOf,
          any.merge(otherConstraints.any)
        )
      case withAnyConstraint: HasAnyConstraint => copy(any = any.merge(withAnyConstraint.any))
      case other => this
    }
  }

  case class StringConstraints(minLength: Option[Int] = None,
                               maxLength: Option[Int] = None,
                               pattern: Option[String] = None,
                               format:  Option[String] = None,
                               any: AnyConstraint = AnyConstraint())
    extends HasAnyConstraint {

    type A = StringConstraints

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.String.MinLength => minLength.map(min => SchemaValue(JsNumber(min)))
      case Keywords.String.MaxLength => maxLength.map(max => SchemaValue(JsNumber(max)))
      case Keywords.String.Pattern => pattern.map(p => SchemaValue(JsString(p)))
      case Keywords.String.Format => format.map(f => SchemaValue(JsString(f)))
      case other => any.resolvePath(other)
    }

    override def merge(otherConstraints: Constraint): StringConstraints = otherConstraints match {
        // TODO: somewhat arbitrary choices here
      case otherStringConstraints: StringConstraints => StringConstraints(
        minLength orElse otherStringConstraints.minLength,
        maxLength orElse otherStringConstraints.maxLength,
        pattern orElse otherStringConstraints.pattern,
        format orElse otherStringConstraints.format,
        any.merge(otherStringConstraints.any)
      )
      case withAnyConstraints: HasAnyConstraint => copy(any = any.merge(withAnyConstraints.any))
      case other => this
    }
  }
}
