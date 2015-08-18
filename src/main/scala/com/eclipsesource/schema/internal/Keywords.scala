package com.eclipsesource.schema.internal

object Keywords {

  val Id = "id"
  val Schema = "$schema"
  val Description = "description"

  val Default = "default"

  object Object {
    val Properties = "properties"
    val PatternProperties = "patternProperties"
    val AdditionalProperties = "additionalProperties"
    val Required = "required"
    val Dependencies = "dependencies"
    val MinProperties = "minProperties"
    val MaxProperties = "maxProperties"
    val Ref = "$ref"
  }

  object Any {
    val AllOf = "allOf"
    val AnyOf = "anyOf"
    val OneOf = "oneOf"
    val Not = "not"
    val Definitions = "definitions"
    val Enum = "enum"
    val Type = "type"
  }

  object Number {
    val Min = "minimum"
    val Max = "maximum"
    val ExclusiveMin = "exclusiveMinimum"
    val ExclusiveMax = "exclusiveMaximum"
    val MultipleOf = "multipleOf"
  }

  object String {
    val MinLength = "minLength"
    val MaxLength = "maxLength"
    val Pattern = "pattern"
  }

  object Array {
    val AdditionalItems = "additionalItems"
    val MinItems = "minItems"
    val MaxItems = "maxItems"
    val UniqueItems = "uniqueItems"
    val Items = "items"
  }

  val ofObject = List(
    Keywords.Id,
    Keywords.Schema,
    Keywords.Description,

    Keywords.Default,
    Keywords.Object.Ref,
    Keywords.Object.Properties,
    Keywords.Object.PatternProperties,
    Keywords.Object.AdditionalProperties,
    Keywords.Object.Required,
    Keywords.Object.Dependencies,
    Keywords.Any.AllOf,
    Keywords.Any.AnyOf,
    Keywords.Any.OneOf,
    Keywords.Any.Type
  )
}
