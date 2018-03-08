package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class PatternPropertiesSpec extends Specification with JsonSpec {

  "patternProperties draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("patternProperties", "draft4")
  }

  "patternProperties draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("patternProperties", "draft7")
  }
}
