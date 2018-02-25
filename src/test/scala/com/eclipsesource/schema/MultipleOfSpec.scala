package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MultipleOfSpec extends Specification with JsonSpec {

  implicit val validator: SchemaValidator = SchemaValidator(Version4)

  "multipleOf draft4" in {
    import Version4._
    validate("multipleOf")
  }

  "multipleOf draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Version7)
    validate("multipleOf", "draft7")
  }
}
