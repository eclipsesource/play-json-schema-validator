package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.data.mapping.VA
import play.api.libs.json._

class ErrorReportingSpec extends Specification {

  case class Post(id: Long, title: String, body: String)

  "Validator" should {

    "handle multiple errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"properties": {
          |  "id":    { "type": "integer" },
          |  "title": { "type": "string", "minLength": 3, "pattern": "^[A-Z].*" }
          |}
          |}""".stripMargin).get
      val result: VA[JsValue] = SchemaValidator.validate(schema)(Json.obj("title" -> "a"))
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "msgs").get.as[JsArray].value.size == 2 }
      true must beTrue
    }

    "handle multiple required errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"properties": {
          |  "id":    { "type": "integer" },
          |  "foo":   { "type": "string"  },
          |  "bar":   { "type": "string"  }
          |},
          |"required": ["foo", "bar"]
          |}""".stripMargin).get
      val result: VA[JsValue] = SchemaValidator.validate(schema)(Json.obj())
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => error.toJson.as[JsArray].value.size == 2 }
      true must beTrue
    }

    "handle nested multiple required errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"properties": {
          |  "id":    { "type": "integer" },
          |  "foo":   { "type": "string"  },
          |  "bar":   {
          |    "type": "object",
          |    "properties": {
          |      "quux": { "type": "integer" }
          |    },
          |    "required": ["quux"]
          |  }
          |},
          |"required": ["foo", "bar"]
          |}""".stripMargin).get
      val result: VA[JsValue] = SchemaValidator.validate(schema)(Json.obj("bar" -> Json.obj()))
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => error.toJson.as[JsArray].value.size == 2 }
      true must beTrue
    }
  }

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
    result.asEither must beLeft.like { case error => (error.toJson(0) \ "schemaPath") == JsDefined(JsString("/")) }
  }

  "handle anyOf validation errors" in {
    val schema = JsonSource.schemaFromString(
      """
        |{
        |  "anyOf": [
        |    { "type": "string", "maxLength": 5 },
        |    { "type": "number", "minimum": 0   }
        |  ]
        |}
      """.
        stripMargin).get
    val result: VA[JsValue] = SchemaValidator. validate(schema)(JsString("too long"))
    result.isFailure must beTrue
    result.asEither must beLeft.like { case error => error.toJson(0) \ "schemaPath" == JsDefined(JsString("/")) }
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
    result.asEither must beLeft.like { case error => error.toJson(0) \ "schemaPath" == JsDefined(JsString("/properties/foo")) }
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
    result.asEither must beLeft.like { case error => error.toJson(0) \ "msgs" == JsDefined(JsArray(Seq(JsString("""Instance does not match all schemas""")))) }
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
    result.asEither must beLeft.like { case error => error.toJson(0) \ "msgs" == JsDefined(JsArray(Seq(JsString("""Instance matches more than one schema""")))) }
  }

}
