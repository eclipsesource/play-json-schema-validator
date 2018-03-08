package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MaxPropertiesSpec extends Specification with JsonSpec {

  "maxProperties draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Some(Version4))
    validate("maxProperties", "draft4")
  }

  "maxProperties draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Some(Version7))
    validate("maxProperties", "draft7")
  }
}
