package com.eclipsesource.schema.internal.draft4

import com.eclipsesource.schema.{SchemaArray, SchemaInteger, SchemaNumber, SchemaObject, SchemaString, SchemaTuple, SchemaVersion}
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.draft4.constraints._
import com.eclipsesource.schema.internal.serialization.SchemaWrites
import play.api.libs.json.{Json, OWrites}

trait SchemaWrites4 extends SchemaWrites { self: SchemaVersion =>

  lazy val anyConstraintWrites: OWrites[AnyConstraints] = {
    case AnyConstraints4(schemaTypeAsString, allOf, anyOf, oneOf, definitions, enum, not, desc, id) =>
      asJsObject(Keywords.Any.Type, schemaTypeAsString) ++
        asJsObject("id", id) ++
        asJsObject(Keywords.Any.AllOf, allOf) ++
        asJsObject(Keywords.Any.AnyOf, anyOf) ++
        asJsObject(Keywords.Any.OneOf, oneOf) ++
        asJsObject(Keywords.Any.Definitions, definitions) ++
        asJsObject(Keywords.Any.Enum, enum) ++
        asJsObject(Keywords.Any.Description, desc) ++
        asJsObject(Keywords.Any.Not, not)
  }

  override lazy val objectWrites: OWrites[SchemaObject] = Default.objectWrites(objectConstraintWrites)
  override lazy val stringWrites: OWrites[SchemaString] = Default.stringWrites(stringConstraintWrites)
  override lazy val integerWrites: OWrites[SchemaInteger] = Default.integerWrites(numberConstraintWrites)
  override lazy val numberWrites: OWrites[SchemaNumber] = Default.numberWrites(numberConstraintWrites)
  override lazy val arrayWrites: OWrites[SchemaArray] = Default.arrayWrites(arrayConstraintWrites)
  override lazy val tupleWrites: OWrites[SchemaTuple] = Default.tupleWrites(arrayConstraintWrites)

  lazy val objectConstraintWrites: OWrites[ObjectConstraints] = {
    case ObjectConstraints4(additionalProps, dependencies, maxProperties, minProperties, patternProps, required, any) =>
      asJsObject(Keywords.Object.AdditionalProperties, additionalProps) ++
        asJsObject(Keywords.Object.Dependencies, dependencies) ++
        asJsObject(Keywords.Object.MaxProperties, maxProperties) ++
        asJsObject(Keywords.Object.MinProperties, minProperties) ++
        asJsObject(Keywords.Object.PatternProperties, patternProps) ++
        asJsObject(Keywords.Object.Required, required) ++
        anyConstraintWrites.writes(any)
  }

  lazy val arrayConstraintWrites: OWrites[ArrayConstraints] = {
    case ArrayConstraints4(maxItems, minItems, additionalItems,  unique, any) =>
      asJsObject(Keywords.Array.AdditionalItems, additionalItems) ++
        asJsObject(Keywords.Array.MaxItems, maxItems) ++
        asJsObject(Keywords.Array.MinItems, minItems) ++
        asJsObject(Keywords.Array.UniqueItems, unique) ++
        anyConstraintWrites.writes(any)
  }

  lazy val stringConstraintWrites: OWrites[StringConstraints] = {
    case StringConstraints4(minLength, maxLength, pattern, format, any) =>
      asJsObject(Keywords.String.MinLength, minLength) ++
        asJsObject(Keywords.String.MaxLength, maxLength) ++
        asJsObject(Keywords.String.Pattern, pattern) ++
        asJsObject(Keywords.String.Format, format) ++
        anyConstraintWrites.writes(any)
  }


  lazy val numberConstraintWrites: OWrites[NumberConstraints] = {
    case NumberConstraints4(min, max, multipleOf, format, any) => Json.obj()
      max.fold(emptyJsonObject)(max => max.isExclusive match {
        case Some(isExclusive) => Json.obj(Keywords.Number.Max -> max.max, Keywords.Number.ExclusiveMax -> isExclusive)
        case _ => Json.obj(Keywords.Number.Max -> max.max)
      }) ++
        min.fold(emptyJsonObject)(min => min.isExclusive match {
          case Some(isExclusive) => Json.obj(Keywords.Number.Min -> min.min, Keywords.Number.ExclusiveMin -> isExclusive)
          case _ => Json.obj(Keywords.Number.Min -> min.min)
        }) ++
        multipleOf.fold(emptyJsonObject)(multipleOf =>
          Json.obj(Keywords.Number.MultipleOf -> multipleOf)
        ) ++ anyConstraintWrites.writes(any) ++
        format.fold(emptyJsonObject)(formatName =>
          Json.obj(Keywords.String.Format -> formatName)
        )
  }
}
