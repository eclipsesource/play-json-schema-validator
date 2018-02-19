package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class ExclusiveMaximumSpec extends Specification with JsonSpec {
  implicit val validator: SchemaValidator = SchemaValidator(Version7)
  import Version7._
  validate("exclusiveMaximum", "draft7")
}
