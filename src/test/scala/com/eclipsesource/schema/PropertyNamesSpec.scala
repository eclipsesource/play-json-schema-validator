package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class PropertyNamesSpec extends Specification with JsonSpec {

  implicit val validator: SchemaValidator = SchemaValidator(Version7)

  "propertyNames draft7" in {
    import Version7._
    validate("propertyNames", "draft7")
  }

}
