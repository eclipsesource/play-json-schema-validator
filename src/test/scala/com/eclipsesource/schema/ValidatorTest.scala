package com.eclipsesource.schema

import com.eclipsesource.schema.test.JSONSource
import org.specs2.mutable.Specification
import play.api.data.mapping.VA
import play.api.libs.json.Json

class ValidatorTest extends Specification {

  case class Post(id: Long, title: String, body: String)

  "Validator" should {

    def schema = JSONSource.schemaFromString(
      """{
        |"properties": {
        |  "id":    { "type": "integer" },
        |  "title": { "type": "string" },
        |  "body":  { "type": "string" }
        |}
    }""".stripMargin).get

    "validate" in {

      val json = Json.obj(
        "id" -> 42,
        "title" -> "Hello",
        "body"  -> "This is some text"
      )

      val post: VA[Post] = SchemaValidator.validate[Post](schema, json, Json.format[Post])

      post.asOpt must beSome.which(_.id == 42)
      post.asOpt must beSome.which(_.title == "Hello")
    }
  }

}
