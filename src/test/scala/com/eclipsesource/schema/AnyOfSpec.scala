package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class AnyOfSpec extends Specification with JsonSpec {

  "anyOf draft4" in {
    import com.eclipsesource.schema.internal.draft4.Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("anyOf", "draft4")
    validate("anyOf", "ajv_tests")
  }

  "anyOf draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("anyOf", "draft7")
  }
}