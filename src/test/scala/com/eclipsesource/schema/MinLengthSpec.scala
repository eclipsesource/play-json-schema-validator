package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

import play.api.libs.json.JsString

class MinLengthSpec extends Specification with JsonSpec {
  validate("minLength")

  "MinLength" should {

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

      SchemaValidator.validate(schema)(JsString("12")).isError must beTrue
    }

  }
}

