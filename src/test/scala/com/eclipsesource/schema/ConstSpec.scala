package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class ConstSpec extends Specification with JsonSpec {

  "const draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Some(Version7))
    validate("const", "draft7")
  }

}
