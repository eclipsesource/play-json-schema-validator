package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class UniqueItemsSpec extends Specification with JsonSpec {

  "uniqueItems draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Version4)
    validate("uniqueItems")
  }

  "uniqueItems draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Version7)
    validate("uniqueItems", "draft7")
  }
}

