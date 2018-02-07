package com.eclipsesource.schema.internal

object Keywords {

  val Schema = "$schema"
  val Description = "description"

  val Default = "default"
  val Ref = "$ref"

  object Object {
    val Properties = "properties"
    val PatternProperties = "patternProperties"
    val AdditionalProperties = "additionalProperties"
    val Required = "required"
    val Dependencies = "dependencies"
    val MinProperties = "minProperties"
    val MaxProperties = "maxProperties"
    val PropertyNames = "propertyNames"
  }

  object Any {
    val AllOf = "allOf"
    val AnyOf = "anyOf"
    val OneOf = "oneOf"
    val Not = "not"
    val Definitions = "definitions"
    val Description = "description"
    val Enum = "enum"
    val Type = "type"
    val If = "if"
    val Then = "then"
    val Else = "else"
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
    val Format = "format"
  }

  object Array {
    val AdditionalItems = "additionalItems"
    val MinItems = "minItems"
    val MaxItems = "maxItems"
    val UniqueItems = "uniqueItems"
    val Items = "items"
    val Contains = "contains"
  }

}