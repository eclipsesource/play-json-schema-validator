package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version4
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class Issue99Spec extends Specification { self =>

  "Issue 99 Spec" should {

    import Version4._

    "validate issue 99 test case" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "object",
          |  "properties": {
          |    "mything": { "$ref": "#thing" }
          |  },
          |  "definitions": {
          |    "thing": {
          |      "id": "#thing",
          |      "type": "string"
          |    }
          |  }
          |}
        """.stripMargin).get
      val validator = SchemaValidator(Some(Version4))
      validator.validate(schema, Json.obj(
        "mything" -> "test"
      )).isSuccess must beTrue
    }

    "validate issue 99-1 test case via URL" in {
      val schemaUrl = self.getClass.getResource("/issue-99-1.json")
      val validator = SchemaValidator(Some(Version4))
      val result = validator.validate(schemaUrl)(Json.obj(
        "mything" -> "test"
      ))
      result.isSuccess must beTrue
    }

    "not cause stack overflow for issue 99-5 test case via URL" in {
      val schemaUrl = self.getClass.getResource("/issue-99-5.json")
      val validator = SchemaValidator(Some(Version4))
      // must terminate
      validator.validate(schemaUrl)(Json.obj(
        "mything" -> "test"
      )).isError must beTrue
    }
  }

}
