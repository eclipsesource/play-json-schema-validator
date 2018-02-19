package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MinimumSpec extends Specification with JsonSpec {

  "minimum draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("minimum")
  }

  "minimum draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("minimum", "draft7")
  }
}
