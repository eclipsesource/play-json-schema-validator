package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import java.net.URL

import play.api.libs.json.JsString

class PatternSpec extends Specification {

  "Pattern" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/pattern.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

  "pattern validation" should {
    val schema = JSONSource.schemaFromString("""{"pattern": "^a*$"}""").get

    "a non-matching pattern is invalid" in {
      val data = JsString("abc")
      val res = Validator.validate(schema, data)
      res.isFailure must beTrue
    }
  }
}
