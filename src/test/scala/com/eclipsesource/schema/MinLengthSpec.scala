package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import java.net.URL

import play.api.libs.json.JsString

class MinLengthSpec extends Specification {

  "MinLength" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/minLength.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }

    "validate against numeric strings that are long enough" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"minLength": 3
        }""".stripMargin).get

      SchemaValidator.validate(schema)(JsString("123")).isSuccess must beTrue
    }

    "not validate against numeric strings that are too short" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"minLength": 3
        }""".stripMargin).get

      SchemaValidator.validate(schema)(JsString("12")).isFailure must beTrue
    }

  }
}

