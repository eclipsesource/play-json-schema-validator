package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JsonSpec, JSONSource}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsString, JsNumber, Json, JsNull}


class AnyOfSpec extends Specification {

  "AnyOf" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/anyOf.json")

      "validate" in {
        foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
      }
    }
  }
}
//
//    val schema = JSONSource.fromString(
//      """{
//        |"anyOf": [
//        |  { "type": "integer" },
//        |  { "minimum": 2 }
//        |]
//      }""".stripMargin
//    )
//
//    "first anyOf valid" in {
//      val data = JsNumber(1)
//      Validator.validate(schema)(data).isSuccess must beTrue
//    }
//
//    "second anyOf valid" in {
//      val data = JsNumber(2.5)
//      Validator.validate(schema)(data).isSuccess must beTrue
//    }
//
//    "both anyOf valid" in {
//      val data = JsNumber(3)
//      Validator.validate(schema)(data).isSuccess must beTrue
//    }
//
//    "neither anyOf valid" in {
//      val data = JsNumber(1.5)
//      val result = Validator.validate(schema)(data)
//      result.isFailure must beTrue
//    }
//  }
//
//  "anyOf with base schema" should {
//
//    val schema = JSONSource.fromString(
//      """{
//        |"type": "string",
//        |"anyOf": [
//        |  {
//        |    "maxLength": 2
//        |  },
//        |  {
//        |    "minLength": 4
//        |  }
//        |]
//      }""".stripMargin)
//
//    "mismatch base schema" in {
//      val data = JsNumber(3)
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//
//    "one anyOf valid" in {
//      val data = JsString("foobar")
//      Validator.validate(schema)(data).isSuccess must beTrue
//    }
//
//    "both anyOf invalid" in {
//      val data = JsString("foo")
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//  }
//
//}
