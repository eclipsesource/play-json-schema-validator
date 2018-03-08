package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version4
import org.specs2.mutable.Specification

class SchemaSpec extends Specification { self =>

  "Schema draft v7" should {
    "validate itself" in {
      import com.eclipsesource.schema.drafts.Version7._
      val schema = JsonSource.fromUrl(self.getClass.getResource("/refs/json-schema-draft-07.json")).get
      val jsonSchema = JsonSource.schemaFromStream(self.getClass.getResourceAsStream("/refs/json-schema-draft-07.json")).get
      implicit val validator: SchemaValidator = SchemaValidator()
      validator.validate(jsonSchema, schema).isSuccess must beTrue
    }
  }

  "Schema draft v4" should {
    "validate itself" in {
      import Version4._
      val schema = JsonSource.fromUrl(self.getClass.getResource("/refs/json-schema-draft-04.json")).get
      val jsonSchema = JsonSource.schemaFromStream(self.getClass.getResourceAsStream("/refs/json-schema-draft-04.json")).get
      implicit val validator: SchemaValidator = SchemaValidator()
      validator.validate(jsonSchema, schema).isSuccess must beTrue
    }
  }
}
