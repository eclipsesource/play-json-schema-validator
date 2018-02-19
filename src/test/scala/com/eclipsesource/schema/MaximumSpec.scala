package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MaximumSpec extends Specification  with JsonSpec {

  "maximum draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("maximum")
  }

  "maximum draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("maximum", "draft7")
  }
}
