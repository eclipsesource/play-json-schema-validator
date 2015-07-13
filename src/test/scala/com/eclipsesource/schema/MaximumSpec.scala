package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import java.net.URL
import play.api.libs.json.{JsString, JsNumber, JsResult, Json}

class MaximumSpec extends Specification {

  "Maximum" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/maximum.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

  "maximum validation" should {

    val schema = JSONSource.schemaFromString(
      """{
        |"maximum": 3.0
      }""".stripMargin).get

    println(Json.prettyPrint(Json.toJson(schema)))

    "ignores non-number" in {
      val data = JsString("x")
      val result = Validator.validate(schema, data)
      println(result)
      result.isSuccess must beTrue
    }
  }

}
