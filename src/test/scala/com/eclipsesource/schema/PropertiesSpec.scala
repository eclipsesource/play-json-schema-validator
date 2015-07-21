package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import java.net.URL

import play.api.libs.json.Json

class PropertiesSpec extends Specification {

  "Properties" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/properties.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }

    "properties, patternProperties, additionalProperties interaction" should {
      val schema = JSONSource.schemaFromString("""{
        "properties": {
          "foo": {"type": "array", "maxItems": 3},
          "bar": {"type": "array"}
        },
        "patternProperties": {"f.o": {"minItems": 2}},
        "additionalProperties": {"type": "integer"}
      }""".stripMargin).get

      "patternProperty invalidates property" in {
        val data = Json.obj("foo" -> Json.arr())
        val res = Validator.validate(schema, data)
        res.isFailure must beTrue
      }
    }


  }

}
