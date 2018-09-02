package com.eclipsesource.schema.internal.draft7

import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.draft7.constraints._
import com.eclipsesource.schema.internal.serialization.SchemaWrites
import com.eclipsesource.schema.{SchemaArray, SchemaInteger, SchemaNumber, SchemaObject, SchemaRoot, SchemaString, SchemaTuple, SchemaVersion}
import play.api.libs.json.{Json, OWrites, Writes}

trait SchemaWrites7 extends SchemaWrites { self: SchemaVersion =>

  lazy val anyConstraintWrites: OWrites[AnyConstraints] = {
    case AnyConstraints7(schemaTypeAsString, allOf, anyOf, oneOf, definitions, enum, const, not, desc, id, _if, _then, _else) =>
      asJsObject(Keywords.Any.Type, schemaTypeAsString) ++
        asJsObject("$id", id) ++
        asJsObject(Keywords.Any.AllOf, allOf) ++
        asJsObject(Keywords.Any.AnyOf, anyOf) ++
        asJsObject(Keywords.Any.OneOf, oneOf) ++
        asJsObject(Keywords.Any.Definitions, definitions) ++
        asJsObject(Keywords.Any.Enum, enum) ++
        asJsObject("const", const) ++
        asJsObject(Keywords.Any.Description, desc) ++
        asJsObject(Keywords.Any.Not, not) ++
        asJsObject(Keywords.Any.If, _if) ++
        asJsObject(Keywords.Any.Then, _then) ++
        asJsObject(Keywords.Any.Else, _else)
  }

  override lazy val rootWrites: Writes[SchemaRoot] = Default.rootWrites
  override lazy val objectWrites: OWrites[SchemaObject] = Default.objectWrites(objectConstraintWrites)
  override lazy val stringWrites: OWrites[SchemaString] = Default.stringWrites(stringConstraintWrites)
  override lazy val integerWrites: OWrites[SchemaInteger] = Default.integerWrites(numberConstraintWrites)
  override lazy val numberWrites: OWrites[SchemaNumber] = Default.numberWrites(numberConstraintWrites)
  override lazy val arrayWrites: OWrites[SchemaArray] = Default.arrayWrites(arrayConstraintWrites)
  override lazy val tupleWrites: OWrites[SchemaTuple] = Default.tupleWrites(arrayConstraintWrites)

  lazy val objectConstraintWrites: OWrites[ObjectConstraints] = OWrites[ObjectConstraints] {
    case ObjectConstraints7(additionalProps, dependencies, patternProps, required, minProperties, maxProperties, propertyNames, any) =>
      asJsObject(Keywords.Object.AdditionalProperties, additionalProps) ++
        asJsObject(Keywords.Object.Dependencies, dependencies) ++
        asJsObject(Keywords.Object.MaxProperties,maxProperties) ++
        asJsObject(Keywords.Object.MinProperties, minProperties) ++
        asJsObject(Keywords.Object.PatternProperties, patternProps) ++
        asJsObject(Keywords.Object.PropertyNames, propertyNames) ++
        asJsObject(Keywords.Object.Required, required) ++
        anyConstraintWrites.writes(any)
  }

  lazy val arrayConstraintWrites: OWrites[ArrayConstraints] = {
    case ArrayConstraints7(maxItems, minItems, additionalItems, contains, unique, any) =>
      asJsObject(Keywords.Array.AdditionalItems, additionalItems) ++
        asJsObject(Keywords.Array.MaxItems, maxItems) ++
        asJsObject(Keywords.Array.MinItems, minItems) ++
        asJsObject(Keywords.Array.Contains, contains) ++
        asJsObject(Keywords.Array.UniqueItems, unique) ++
        anyConstraintWrites.writes(any)
  }

  lazy val numberConstraintWrites: OWrites[NumberConstraints] = {
    case NumberConstraints7(min, max, multipleOf, format, any) =>
      max.fold(emptyJsonObject)(max => max.isExclusive match {
        case Some(_) => Json.obj(Keywords.Number.ExclusiveMax -> max.max)
        case _ => Json.obj(Keywords.Number.Max -> max.max)
      }) ++
        min.fold(emptyJsonObject)(min => min.isExclusive match {
          case Some(_) => Json.obj(Keywords.Number.ExclusiveMin -> min.min)
          case _ => Json.obj(Keywords.Number.Min -> min.min)
        }) ++
        multipleOf.fold(emptyJsonObject)(multipleOf =>
          Json.obj(Keywords.Number.MultipleOf -> multipleOf)
        ) ++ anyConstraintWrites.writes(any) ++
        format.fold(emptyJsonObject)(formatName =>
          Json.obj(Keywords.String.Format -> formatName)
        )
  }

  lazy val stringConstraintWrites: OWrites[StringConstraints] = {
    case StringConstraints7(minLength, maxLength, pattern, format, any) =>
      asJsObject(Keywords.String.MinLength, minLength) ++
        asJsObject(Keywords.String.MaxLength, maxLength) ++
        asJsObject(Keywords.String.Pattern, pattern) ++
        asJsObject(Keywords.String.Format, format) ++
        anyConstraintWrites.writes(any)

  }
}
