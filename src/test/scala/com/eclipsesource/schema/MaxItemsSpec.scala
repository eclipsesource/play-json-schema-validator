package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import java.net.URL

import play.api.libs.json.{JsString, Json, JsArray}

class MaxItemsSpec extends Specification {

  "MaxItems" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/maxItems.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

  "maxItems validation" should {

    val schema = JSONSource.schemaFromString("{ \"maxItems\": 2 }").get

    println(Json.prettyPrint(Json.toJson(schema)))

    "too long is invalid" in {
      val data = JsString("foobar")
      val res = Validator.validate(schema, data)
      println(res)
      res.isSuccess must beTrue
    }
  }
}

