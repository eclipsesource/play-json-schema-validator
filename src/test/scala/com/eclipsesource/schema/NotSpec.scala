package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7

class NotSpec extends Specification with JsonSpec {

  "not draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Version4)
    validate("not")
  }

  "not draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Version7)
    validate("not", "draft7")
  }
}
