package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class AnyOfSpec extends Specification with JsonSpec {

  "anyOf draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("anyOf")
    validate("anyOf", "ajv_tests")
  }

  "anyOf draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("anyOf", "draft7")
  }
}