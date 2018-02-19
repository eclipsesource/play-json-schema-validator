package com.eclipsesource.schema

import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.constraints.IsResolvable
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import play.api.libs.json.{JsArray, JsBoolean, JsNumber, JsString}

trait SchemaKeywords {
  def id: String

  /** @version 7 */
  def contains: Option[String]
  /** @version 7 */
  def If: Option[String]
  /** @version 7 */
  def Then: Option[String]
  /** @version 7 */
  def Else: Option[String]
  /** @version 7 */
  def propertyNames: Option[String]

  def ofArray: Set[String]
  def ofObject: Set[String]
  def ofTuple: Set[String] = ofArray


  val defaultObjectKeywords = Set(
    Keywords.Schema,
    Keywords.Description,
    Keywords.Default,
    Keywords.Ref,

    Keywords.Object.Properties,
    Keywords.Object.PatternProperties,
    Keywords.Object.AdditionalProperties,
    Keywords.Object.Required,
    Keywords.Object.Dependencies,
    id,
    Keywords.Any.AllOf,
    Keywords.Any.AnyOf,
    Keywords.Any.OneOf,
    Keywords.Any.Type,
    Keywords.Any.If,
    Keywords.Any.Then,
    Keywords.Any.Else
  )

  val defaultArrayKeywords = Set(
    Keywords.Array.AdditionalItems,
    Keywords.Array.Items,
    Keywords.Array.MaxItems,
    Keywords.Array.MinItems,
    Keywords.Array.UniqueItems,
    id,
    Keywords.Any.AllOf,
    Keywords.Any.AnyOf,
    Keywords.Any.OneOf,
    Keywords.Any.Type
  )
}

sealed trait SchemaVersion extends JSONSchemaReads with JSONSchemaWrites {

  def keywords: SchemaKeywords

  implicit val anyConstraintIsResolvable: IsResolvable[AnyConstraint] =
    (constraint: AnyConstraint, path: String) => path match {
      case Keywords.Any.Type  => constraint.schemaTypeAsString.map(t => SchemaValue(JsString(t)))
      case Keywords.Any.AllOf => constraint.allOf.map(types => SchemaTuple(types))
      case Keywords.Any.AnyOf => constraint.anyOf.map(types => SchemaTuple(types))
      case Keywords.Any.OneOf => constraint.oneOf.map(types => SchemaTuple(types))
      case Keywords.Any.Definitions => constraint.definitions.map(entries =>
        SchemaMap(
          Keywords.Any.Definitions,
          entries.toSeq.map { case (name, schema) => SchemaProp(name, schema) })
      )
      case Keywords.Any.Enum => constraint.enum.map(e => SchemaValue(JsArray(e)))
      case Keywords.Any.Not => constraint.not
      case other if other == keywords.id => constraint.id.map(id => SchemaValue(JsString(id)))
      case _ => None
    }

  implicit def objectConstrainsAreResolvable(implicit anyConstraintIsResolvable: IsResolvable[AnyConstraint]): IsResolvable[ObjectConstraints] =
    (constraint: ObjectConstraints, path: String) => path match {
      case Keywords.Object.AdditionalProperties => constraint.additionalProps
      case Keywords.Object.Dependencies => constraint.dependencies.map(entries =>
        SchemaMap(Keywords.Object.Dependencies, entries.toSeq.map(e => SchemaProp(e._1, e._2)))
      )
      case Keywords.Object.PatternProperties => constraint.patternProps.map(patternProps => SchemaObject(patternProps.toSeq.map(e => SchemaProp(e._1, e._2))))
      case Keywords.Object.MinProperties => constraint.minProperties.map(min => SchemaValue(JsNumber(min)))
      case Keywords.Object.MaxProperties => constraint.maxProperties.map(max => SchemaValue(JsNumber(max)))
      case other => anyConstraintIsResolvable.resolvePath(constraint.any, other)
    }

  implicit def arrayConstrainsAreResolvable(implicit anyConstraintIsResolvable: IsResolvable[AnyConstraint]): IsResolvable[ArrayConstraints] =
    (constraint: ArrayConstraints, path: String) => path match {
      case Keywords.Array.MinItems => constraint.minItems.map(min => SchemaValue(JsNumber(min)))
      case Keywords.Array.MaxItems => constraint.maxItems.map(max => SchemaValue(JsNumber(max)))
      case Keywords.Array.AdditionalItems => constraint.additionalItems
      case Keywords.Array.UniqueItems => constraint.unique.map(u => SchemaValue(JsBoolean(u)))
      case other => anyConstraintIsResolvable.resolvePath(constraint.any, other)
    }

  implicit def numberConstraintIsResolvable(implicit anyConstraintIsResolvable: IsResolvable[AnyConstraint]): IsResolvable[NumberConstraints] =
    (constraint: NumberConstraints, path: String) => path match {
      case Keywords.Number.Min => constraint.min.map(m => SchemaValue(JsNumber(m.min)))
      case Keywords.Number.Max => constraint.max.map(m => SchemaValue(JsNumber(m.max)))
      case Keywords.Number.MultipleOf => constraint.multipleOf.map(m => SchemaValue(JsNumber(m)))
      case Keywords.String.Format => constraint.format.map(f => SchemaValue(JsString(f)))
      case other => anyConstraintIsResolvable.resolvePath(constraint.any, other)
    }

  implicit def stringConstraintIsResolvable(implicit anyConstraintIsResolvable: IsResolvable[AnyConstraint]): IsResolvable[StringConstraints] =
    (constraint: StringConstraints, path: String) => path match {
      case Keywords.String.MinLength => constraint.minLength.map(min => SchemaValue(JsNumber(min)))
      case Keywords.String.MaxLength => constraint.maxLength.map(max => SchemaValue(JsNumber(max)))
      case Keywords.String.Pattern => constraint.pattern.map(p => SchemaValue(JsString(p)))
      case Keywords.String.Format => constraint.format.map(f => SchemaValue(JsString(f)))
      case other => anyConstraintIsResolvable.resolvePath(constraint.any, other)
    }

  implicit def hasAnyConstraintIsResolvable(implicit anyConstraintIsResolvable: IsResolvable[AnyConstraint]): IsResolvable[HasAnyConstraint] =
    (constraint: HasAnyConstraint, path: String) => anyConstraintIsResolvable.resolvePath(constraint.any, path)
}

trait Version4 extends SchemaVersion {
  override def keywords: SchemaKeywords = new SchemaKeywords {
    override def id: String = "id"
    override def contains: Option[String] = None

    override def ofArray: Set[String] = defaultArrayKeywords

    override def ofObject: Set[String] = defaultObjectKeywords

    /** @version 7 */
    override def If: Option[String] = None

    /** @version 7 */
    override def Then: Option[String] = None

    /** @version 7 */
    override def Else: Option[String] = None

    /** @version 7 */
    override def propertyNames: Option[String] = None
  }

}

object Version4 extends Version4

trait Version7 extends SchemaVersion {
  override def keywords: SchemaKeywords = new SchemaKeywords {
    override val ofArray: Set[String] = defaultArrayKeywords ++ Set(Keywords.Array.Contains)
    override val ofObject: Set[String] = defaultObjectKeywords ++ Set(Keywords.Object.PropertyNames)

    override def id: String = "$id"

    /** @version 7 */
    override def contains: Option[String] = Some(Keywords.Array.Contains)

    /** @version 7 */
    override def If: Option[String] = Some("if")

    /** @version 7 */
    override def Then: Option[String] = Some("then")

    /** @version 7 */
    override def Else: Option[String] = Some("else")

    /** @version 7 */
    override def propertyNames: Option[String] = Some(Keywords.Object.PropertyNames)
  }
}

object Version7 extends Version7
