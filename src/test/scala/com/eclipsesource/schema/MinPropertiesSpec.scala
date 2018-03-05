package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MinPropertiesSpec extends Specification with JsonSpec {

  "minProperties draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Some(Version4))
    validate("minProperties", "draft4")
  }

  "minProperties draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Some(Version7))
    validate("minProperties", "draft7")
  }
}
