package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.data.mapping.{Failure, VA}
import play.api.libs.json._

class ErrorReportingSpec extends Specification {

  case class Post(id: Long, title: String, body: String)

  "Validator" should {

    def schema = JsonSource.schemaFromString(
      """{
        |"properties": {
        |  "id":    { "type": "integer" },
        |  "title": { "type": "string" },
        |  "body":  { "type": "string" }
        |}
    }""".stripMargin).get

    "handle anyOf validation errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"anyOf": [
          |  {
          |    "type": "integer"
          |  },
          |  {
          |    "minimum": 2
          |  }
          |]
          |}""".stripMargin).get
      val result: VA[JsValue] = SchemaValidator.validate(schema)(JsNumber(1.5))
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => error.toJson \ "schemaPath" == JsDefined(JsString("/")) }
    }

    "handle nested anyOf validation errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"properties": {
          |  "foo": {
          |    "anyOf": [
          |      {
          |        "type": "integer"
          |      },
          |      {
          |        "minimum": 2
          |      }
          |    ]
          |  }
          |}
          |}""".stripMargin).get
      val result: VA[JsValue] = SchemaValidator.validate(schema)(Json.obj("foo" -> 1.5))
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => error.toJson \ "schemaPath" == JsDefined(JsString("/properties/foo")) }
    }

    "handle allOf validation errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"allOf": [
          |  {
          |    "properties": {
          |      "bar": {"type": "integer"}
          |    },
          |    "required": ["bar"]
          |  },
          |  {
          |    "properties": {
          |      "foo": {"type": "string"}
          |    },
          |    "required": ["foo"]
          |  }
          |]
          |}""".stripMargin).get
      val result: VA[JsValue] = SchemaValidator.validate(schema)(Json.obj("foo" -> "baz"))
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => error.toJson \ "msg" == JsDefined(JsString("""{"foo":"baz"} does not match all schemas""")) }
    }

    "handle oneOf validation errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"oneOf": [
          |  {
          |    "type": "integer"
          |  },
          |  {
          |    "minimum": 2
          |  }
          |]
          |}""".stripMargin).get

      val result: VA[JsValue] = SchemaValidator.validate(schema)(JsNumber(3))
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => error.toJson \ "msg" == JsDefined(JsString("""3 does match more than one schema""")) }
    }
  }

}
