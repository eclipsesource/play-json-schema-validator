package com.eclipsesource.schema.internal.draft7

import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.draft7.constraints._
import com.eclipsesource.schema.internal.serialization.SchemaWrites
import com.eclipsesource.schema.{SchemaArray, SchemaInteger, SchemaNumber, SchemaObject, SchemaString, SchemaTuple, SchemaVersion}
import play.api.libs.json.{JsObject, Json, OWrites}

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
        // TODO
        asJsObject("const", const) ++
        asJsObject(Keywords.Any.Description, desc) ++
        asJsObject(Keywords.Any.Not, not) ++
        asJsObject(Keywords.Any.If, _if) ++
        asJsObject(Keywords.Any.Then, _then) ++
        asJsObject(Keywords.Any.Else, _else)
  }

  // TODO: default is missing
  override val objectWrites: OWrites[SchemaObject] = OWrites[SchemaObject] { obj =>

    val props = obj.properties.map(attr => attr.name -> Json.toJson(attr.schemaType))
    val remainingProps = obj.otherProps.map(attr => attr._1 -> Json.toJson(attr._2))

    // TODO: only write none empty seq of properties
    val o = (if (props.nonEmpty) Json.obj("properties" -> JsObject(props)) else Json.obj()).deepMerge(JsObject(remainingProps))
    o.deepMerge(objectConstraintWriter.writes(obj.constraints))
  }

  override lazy val stringWrites: OWrites[SchemaString] = OWrites[SchemaString] { s =>
    val stringConstraints = stringConstraintWriter.writes(s.constraints)
    if (stringConstraints.fields.isEmpty) Json.obj("type" -> "string")
    else stringConstraints
  }

  override lazy val integerWrites: OWrites[SchemaInteger] = OWrites[SchemaInteger] { i =>
    val integerConstraints = numberConstraintWriter.writes(i.constraints)
    if (integerConstraints.fields.isEmpty) Json.obj("type" -> "integer")
    else integerConstraints
  }

  override lazy val numberWrites: OWrites[SchemaNumber] = OWrites[SchemaNumber] { num =>
    val numberConstraints = numberConstraintWriter.writes(num.constraints)
    if (numberConstraints.fields.isEmpty) Json.obj("type" -> "number")
    else numberConstraints
  }

  override lazy val arrayWrites: OWrites[SchemaArray] = OWrites[SchemaArray] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.item)
    ) ++ arrayConstraintWriter.writes(arr.constraints) ++
      JsObject(arr.otherProps.map(attr => attr._1 -> Json.toJson(attr._2)))
  }

  override val tupleWrites: OWrites[SchemaTuple] = OWrites[SchemaTuple] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.items)
    ) ++ arrayConstraintWriter.writes(arr.constraints)
  }

  lazy val objectConstraintWriter: OWrites[ObjectConstraints] = OWrites[ObjectConstraints] {
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

  lazy val arrayConstraintWriter: OWrites[ArrayConstraints] = {
    case ArrayConstraints7(maxItems, minItems, additionalItems, contains, unique, any) =>
      asJsObject(Keywords.Array.AdditionalItems, additionalItems) ++
        asJsObject(Keywords.Array.MaxItems, maxItems) ++
        asJsObject(Keywords.Array.MinItems, minItems) ++
        asJsObject(Keywords.Array.Contains, contains) ++
        asJsObject(Keywords.Array.UniqueItems, unique) ++
        anyConstraintWrites.writes(any)
  }

  lazy val numberConstraintWriter: OWrites[NumberConstraints] = {
    // TODO: write format?
    case NumberConstraints7(min, max, multipleOf, format, any) =>
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
        ) ++ anyConstraintWrites.writes(any)
  }

  lazy val stringConstraintWriter: OWrites[StringConstraints] = {
    case StringConstraints7(minLength, maxLength, pattern, format, any) =>
      asJsObject(Keywords.String.MinLength, minLength) ++
        asJsObject(Keywords.String.MaxLength, maxLength) ++
        asJsObject(Keywords.String.Pattern, pattern) ++
        asJsObject(Keywords.String.Format, format) ++
        anyConstraintWrites.writes(any)
  }
}
