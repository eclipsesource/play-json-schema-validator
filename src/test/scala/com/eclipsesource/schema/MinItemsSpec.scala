package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MinItemsSpec extends Specification with JsonSpec {

  "minItems draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("minItems")
  }

  "minItems draft7" in {
    import com.eclipsesource.schema.internal.draft7.Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("minItems", "draft7")
  }
}
