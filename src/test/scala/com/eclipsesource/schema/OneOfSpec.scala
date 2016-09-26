package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class OneOfSpec extends Specification with JsonSpec {
  validate("oneOf")
  validate("oneOf", "ajv_tests")

  "oneOf must be array of objects (invalid)" in {
    val schema = JsonSource.schemaFromString(
      """{
        | "oneOf": [
        |  "#/definitions/foo"
        | ]
        |}""".stripMargin)
    schema.isError must beTrue
  }

  "oneOf must be array of objects (valid)" in {
    val schema = JsonSource.schemaFromString(
      """{
        | "oneOf": [{
        |  "$ref": "#/definitions/foo"
        | }]
        |}""".stripMargin)
    schema.isSuccess must beTrue
  }
}
