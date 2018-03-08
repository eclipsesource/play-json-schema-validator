package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class OneOfSpec extends Specification with JsonSpec {

  import Version4._

  "oneOf draft4" in {
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("oneOf", "draft4")
    validate("oneOf", "ajv_tests")
  }

  "oneOf draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("oneOf", "draft7")
  }

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
