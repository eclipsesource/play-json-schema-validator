package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class AllOfSpec extends Specification {

  "AllOf" should {

    "validate" in {
      //      val resourceUrl: URL = getClass.getResource("/draft4/allOf.json")
      //      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)

      val schema = JSONSource.schemaFromString(
        """{
          |
          |            "allOf": [
          |                {
          |                    "properties": {
          |                        "bar": {"type": "integer"}
          |                    },
          |                    "required": ["bar"]
          |                },
          |                {
          |                    "properties": {
          |                        "foo": {"type": "string"}
          |                    },
          |                    "required": ["foo"]
          |                }
          |            ]
          |        }
        """.stripMargin).get


      "wrong type" in {
        val data = Json.obj("foo" -> "baz", "bar" -> "quux")
        val res = Validator.validate(schema, data)
        res.isFailure must beTrue
      }
    }
  }
}
