package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class ReferenceSpec extends Specification {

  "References" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/ref.json")

      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

  "remote ref, containing refs itself" should {

    sequential

    val schema = JSONSource.schemaFromString(
      """{
        |"$ref": "http://json-schema.org/draft-04/schema#"
          }""".stripMargin).get

    "remote ref invalid" in {
      val data = Json.obj("minLength" -> -1)
      val result = Validator.validate(schema)(data)
      result.isFailure must beTrue
    }
  }

}
