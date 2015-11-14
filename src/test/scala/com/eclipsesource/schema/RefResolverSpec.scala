package com.eclipsesource.schema

import java.net.URL

import org.specs2.mutable.Specification
import play.api.libs.json.{JsValue, Json}

class RefResolverSpec extends Specification {

  val resourceUrl: URL = getClass.getResource("/talk.json")
  val schema: JsValue = JsonSource.fromURL(resourceUrl).get

  "RefResovler" should {
    "be capable of resolving relative links to files" in {
      val instance = Json.obj(
        "location" -> Json.obj(
          "name" -> "Munich"
        )
      )
      val result = SchemaValidator.validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }
  }

}
