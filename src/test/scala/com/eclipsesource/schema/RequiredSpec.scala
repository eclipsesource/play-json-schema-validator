package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JsonSpec, JSONSource}
import org.specs2.mutable.Specification
import play.api.libs.json.Json
import java.net.URL

class RequiredSpec extends Specification {

  "Required" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/required.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }



  "required validation" should {

    val schema = JSONSource.schemaFromString(
      """{
        |"properties": {
        |  "foo": {},
        |  "bar": {}
        |},
        |"required": ["foo"]
      }""".stripMargin).get

    "present required property is valid" in {
      val data = Json.obj("foo" -> 1)
      val result = SchemaValidator.validate(schema)(data)
      result.isSuccess must beTrue
    }

    "non-present required property is invalid" in {
      val data = Json.obj("bar" -> 1)
      val result = SchemaValidator.validate(schema)(data)
      result.isFailure must beTrue
    }

  }

}
