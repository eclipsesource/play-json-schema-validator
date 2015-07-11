package com.eclipsesource.schema.internal.constraints

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.{Keywords, Context}
import play.api.libs.json.JsValue


object Constraints {

  trait Constraint {
    type Sub <: Constraint
    def updated(fn: SchemaType => SchemaType): Sub
  }

  case class AnyConstraint(allOf: Option[Seq[SchemaType]] = None,
                           anyOf: Option[Seq[SchemaType]] = None,
                           oneOf: Option[Seq[SchemaType]] = None,
                           definitions: Option[Map[String, SchemaType]] = None,
                           enum: Option[Seq[JsValue]] = None)
    extends Constraint with Resolvable {

    override type Sub = AnyConstraint

    override def updated(fn: SchemaType => SchemaType): Sub = {
      copy(
        allOf.map(schemas => schemas.map(fn)),
        anyOf.map(schemas => schemas.map(fn)),
        oneOf.map(schemas => schemas.map(fn)),
        definitions.map(_.map(entry => entry._1 -> fn(entry._2)))
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

  trait HasAnyConstraint {
    def any: AnyConstraint
  }

  case class CompositeConstraints(constraints: Seq[Constraint]) extends Constraint {
    override type Sub = CompositeConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = copy(constraints = constraints.map(c => c.updated(fn)))
  }

  case class ObjectConstraints(additionalProps: Option[SchemaType] = None,
                               dependencies: Option[Map[String, SchemaType]] = None,
                               patternProps: Option[Map[String, SchemaType]] = None,
                               required: Option[Seq[String]] = None,
                               any: AnyConstraint = AnyConstraint(None, None, None, None)) extends Constraint with HasAnyConstraint with Resolvable {
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

  case class ArrayConstraints(maxItems: Option[Int], minItems: Option[Int], additionalItems: Option[SchemaType], unique: Option[Boolean], any: AnyConstraint) extends Constraint with HasAnyConstraint {
    override type Sub = ArrayConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = copy(any = any.updated(fn))
  }

  case class Minimum(min: Double, isExclusive: Option[Boolean])
  case class Maximum(max: Double, isExclusive: Option[Boolean])
  case class MultipleOf(factor: Double)

  case class NumberConstraints(min: Option[Minimum], max: Option[Maximum], multipleOf: Option[Double], any: AnyConstraint) extends Constraint with HasAnyConstraint {
    override type Sub = NumberConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = {
      copy(any = any.updated(fn))
    }
  }

  case class StringConstraints(minLength: Option[Int], maxLength: Option[Int], format: Option[String], any: AnyConstraint) extends Constraint with HasAnyConstraint {
    type Sub = StringConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = {
      copy(any = any.updated(fn))
    }
  }

  case class BooleanConstraints(any: AnyConstraint) extends Constraint with HasAnyConstraint {
    override type Sub = BooleanConstraints

    override def updated(fn: (SchemaType) => SchemaType): Sub = copy(any = any.updated(fn))
  }
}
