package com.eclipsesource.schema.internal.constraints

import play.api.libs.json.JsValue


object Constraints {
import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.{Keywords, Context}

  trait HasAnyConstraint {
    def any: AnyConstraint
  }

  object NoConstraints extends HasAnyConstraint {
    override def any: AnyConstraint = AnyConstraint()
  }

  trait  Constraint {
    type Sub <: Constraint
    def updated(fn: SchemaType => SchemaType): Sub
  }

  case class AnyConstraint(schemaTypeAsString: Option[String] = None,
                           allOf: Option[Seq[SchemaType]] = None,
                           anyOf: Option[Seq[SchemaType]] = None,
                           oneOf: Option[Seq[SchemaType]] = None,
                           definitions: Option[Map[String, SchemaType]] = None,
                           enum: Option[Seq[JsValue]] = None,
                           not: Option[SchemaType] = None)
    extends Constraint with Resolvable {

    override type Sub = AnyConstraint

    override def updated(fn: SchemaType => SchemaType): Sub = {
      copy(
        allOf = allOf.map(_.map(fn)),
        anyOf = anyOf.map(_.map(fn)),
        oneOf = oneOf.map(_.map(fn)),
        definitions = definitions.map(_.map(entry => entry._1 -> fn(entry._2))),
        not  = not.map(fn)
      )
    }

    override def resolvePath(path: String): Option[SchemaType] = path match {
      case Keywords.Any.AllOf => allOf.map(types => SchemaTuple(() => types, types.size))
      case Keywords.Any.AnyOf => anyOf.map(types => SchemaTuple(() => types, types.size))
      case Keywords.Any.OneOf => oneOf.map(types => SchemaTuple(() => types, types.size))
      case Keywords.Any.Definitions => definitions.map(entries => SchemaObject(entries.toSeq.map(e => SchemaAttribute(e._1, e._2))))
      case Keywords.Any.Enum => enum.map(e => SchemaArrayConstant(e))
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

    override type Sub = ObjectConstraints


    def additionalPropertiesOrDefault: SchemaType = {
      additionalProps.fold(ObjectConstraints.emptyObject)(identity)
    }

    override def updated(fn: (SchemaType) => SchemaType): Sub = {
      // TODO: copy ain't type safe
      copy(
        additionalProps.map(t => fn(t)),
        dependencies.map(_.map(dep => dep._1 -> fn(dep._2))),
        patternProps.map(_.map(p => p._1 -> fn(p._2))),
        required,
        minProperties,
        maxProperties,
        any.updated(fn)
      )
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
    override type Sub = ArrayConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = copy(
      additionalItems = additionalItems.map(fn),
      any = any.updated(fn)
    )
  }

  case class Minimum(min: BigDecimal, isExclusive: Option[Boolean])
  case class Maximum(max: BigDecimal, isExclusive: Option[Boolean])
  case class MultipleOf(factor: BigDecimal)


  case class NumberConstraints(min: Option[Minimum] = None,
                               max: Option[Maximum] = None,
                               multipleOf: Option[BigDecimal] = None,
                               any: AnyConstraint = AnyConstraint())
    extends Constraint with HasAnyConstraint {

    override type Sub = NumberConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = {
      copy(any = any.updated(fn))
    }
  }

  case class CompoundConstraints(constraints: Seq[Constraint], any: AnyConstraint) extends Constraint  {
    override type Sub = CompoundConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = {
      copy(
        constraints = constraints.map(_.updated(fn))
      )
    }

  }

  case class StringConstraints( minLength: Option[Int] = None,
                                maxLength: Option[Int] = None,
                                pattern: Option[String] = None,
                                any: AnyConstraint = AnyConstraint())
    extends Constraint with HasAnyConstraint {

    type Sub = StringConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = {
      copy(any = any.updated(fn))
    }
  }

  case class BooleanConstraints(any: AnyConstraint = AnyConstraint()) extends Constraint with HasAnyConstraint {
    override type Sub = BooleanConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = copy(any = any.updated(fn))
  }

  case class NullConstraints(any: AnyConstraint) extends Constraint with HasAnyConstraint {
    override type Sub = NullConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = copy(any = any.updated(fn))
  }
}
