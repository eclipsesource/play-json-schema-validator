package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class DependenciesSpec extends Specification with JsonSpec {

  "dependencies draft 4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("dependencies")
  }

  "dependencies draft 7" in {
    import Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("dependencies", "draft7")
  }
}
