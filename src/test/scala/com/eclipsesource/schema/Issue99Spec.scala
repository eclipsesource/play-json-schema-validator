package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json.Json

class Issue99Spec extends Specification {

  "Issue 99 Spec" should {

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
      val validator = SchemaValidator()
      validator.validate(schema, Json.obj(
        "mything" -> "test"
      )).isSuccess must beTrue
    }

    "validate issue 99-1 test case via URL" in {
      val schemaUrl = getClass.getResource("/issue-99-1.json")
      val validator = SchemaValidator()
      val result = validator.validate(schemaUrl, Json.obj(
        "mything" -> "test"
      ))
      result.isSuccess must beTrue
    }

    "not cause stack overflow for issue 99-5 test case via URL" in {
      val schemaUrl = getClass.getResource("/issue-99-5.json")
      val validator = SchemaValidator()
      // must terminate
      validator.validate(schemaUrl, Json.obj(
        "mything" -> "test"
      )).isError must beTrue
    }
  }

}
