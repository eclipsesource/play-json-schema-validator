package com.eclipsesource.schema.internal

object Keywords {

  val Id = "id"
  val Schema = "$schema"
  val Description = "description"

  val Ref = "$ref"
  val Default = "default"
  val Type = "type"
  val Properties = "properties"
  val PatternProperties = "patternProperties"
  val AdditionalProperties = "additionalProperties"
  val Required = "required"
  val Dependencies = "dependencies"
  val AllOf = "allOf"
  val AnyOf = "anyOf"

  val ofObject = List(
    // TODO these actually only apply to schema
    Keywords.Id,
    Keywords.Schema,
    Keywords.Description,
    //
    // TODO: default is not yet implemented
    Keywords.Default,
    Keywords.Ref,
    Keywords.Type,
    Keywords.Properties,
    Keywords.PatternProperties,
    Keywords.AdditionalProperties,
    Keywords.Required,
    Keywords.Dependencies,
    Keywords.AllOf,
    Keywords.AnyOf
  )
}
