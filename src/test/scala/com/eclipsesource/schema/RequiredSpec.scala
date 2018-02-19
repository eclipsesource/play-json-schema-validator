package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class RequiredSpec extends Specification with JsonSpec {

  "required draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Version4)
    validate("required")
  }

  "required draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Version7)
    validate("required", "draft7")
  }
}
