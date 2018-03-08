package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class BooleanSchemaSpec extends Specification with JsonSpec {

  "boolean scheam draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("boolean_schema", "draft7")
  }

}
