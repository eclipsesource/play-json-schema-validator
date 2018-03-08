package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MaximumSpec extends Specification  with JsonSpec {

  "maximum draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("maximum", "draft4")
  }

  "maximum draft7" in {
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    import Version7._
    validate("maximum", "draft7")
  }
}
