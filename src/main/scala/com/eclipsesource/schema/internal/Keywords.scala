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
    val Ref = "$ref"
  }

  object Any {
    val AllOf = "allOf"
    val AnyOf = "anyOf"
    val OneOf = "oneOf"
    val Definitions = "definitions"
    val Enum = "enum"
    val Type = "type"
  }

  object Number {
    val Max = "maximum"
    val Min = "minimum"
    val ExclusiveMax = "exclusiveMax"
    val ExclusiveMin = "exclusiveMin"
    val MultipleOf = "multipleOf"
  }

  object String {
    val MinLength = "minLength"
    val MaxLength = "maxLength"
    val Format = "format"
  }

  object Array {
    val AdditionalItems = "additionalItems"
  }

  val ofObject = List(
    // TODO these actually only apply to schema
    Keywords.Id,
    Keywords.Schema,
    Keywords.Description,
    //
    // TODO: default is not yet implemented
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
