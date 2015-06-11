package com.eclipsesource.schema

import com.eclipsesource.schema.test.JSONSource
import org.specs2.mutable.Specification
import play.api.libs.json.Json


class ReadsSpec extends Specification {

  val schema = JSONSource.schemaFromString(
    """{
      |"id": "http://json-schema.org/draft-04/schema#",
      |"$schema": "http://json-schema.org/draft-04/schema#"
    }""".stripMargin)

  "Read of id property" should {

    "not emit a ref" in {
      println(schema.get)
      println(Json.prettyPrint(Json.toJson(schema.get)))
      true must beTrue
    }
  }

}
