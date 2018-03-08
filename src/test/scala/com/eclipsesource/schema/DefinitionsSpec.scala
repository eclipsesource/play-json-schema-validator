package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class DefinitionsSpec extends Specification with JsonSpec { self =>

  "validate draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("definitions", "draft4")
  }

  "validate draft7" in {
    import Version7._
    val jsonSchema = JsonSource.schemaFromStream(self.getClass.getResourceAsStream("/refs/json-schema-draft-07.json")).get
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
      .addSchema("http://json-schema.org/draft-07/schema", jsonSchema)
    validate("definitions", "draft7")
  }

}
