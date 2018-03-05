package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class PatternSpec extends Specification with JsonSpec {

  "pattern draft4" in {
    import com.eclipsesource.schema.internal.draft4.Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("pattern", "draft4")
  }

  "pattern draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("pattern", "draft7")
  }
}
