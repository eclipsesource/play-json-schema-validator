package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class DefaultSpec extends Specification with JsonSpec {

  "validate draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("default")
  }

  "validate draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("default", "draft7")
  }
}
