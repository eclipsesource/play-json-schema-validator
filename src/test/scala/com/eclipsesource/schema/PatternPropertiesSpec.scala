package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class PatternPropertiesSpec extends Specification with JsonSpec {

  "patternProperties draft4" in {
    import com.eclipsesource.schema.internal.draft4.Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("patternProperties", "draft4")
  }

  "patternProperties draft7" in {
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    import Version7._
    validate("patternProperties", "draft7")
  }
}
