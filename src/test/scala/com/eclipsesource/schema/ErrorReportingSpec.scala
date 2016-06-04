package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.data.validation.ValidationError
import play.api.libs.json._

class ErrorReportingSpec extends Specification {

  case class Post(id: Long, title: String, body: String)

  val validator = SchemaValidator()

  "Validator" should {

    "handle multiple errors for same property" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"properties": {
          |  "id":    { "type": "integer" },
          |  "title": { "type": "string", "minLength": 3, "pattern": "^[A-Z].*" }
          |}
          |}""".stripMargin).get
      val result: JsResult[JsValue] = validator.validate(schema)(Json.obj("title" -> "a"))
      result.isError must beTrue
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "msgs").get.as[JsArray].value.size == 2 }
    }

    "handle multiple errors for different properties" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"properties": {
          |  "id":    { "type": "integer" },
          |  "title": { "type": "string", "minLength": 3 },
          |  "foo"  : { "type": "string", "pattern": "^[A-Z].*" }
          |}
          |}""".stripMargin).get
      val result: JsResult[JsValue] = validator.validate(schema)(Json.obj("title" -> "a", "foo" -> "a"))
      result.asEither must beLeft.like { case error =>
        error.toJson.value must haveLength(2)
      }
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
      val result: JsResult[JsValue] = validator.validate(schema)(Json.obj())
      result.isError must beTrue
      result.asEither must beLeft.like { case error => error.toJson.as[JsArray].value.size == 2 }
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
      val result: JsResult[JsValue] = validator.validate(schema)(Json.obj("bar" -> Json.obj()))
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        error.toJson.value must haveLength(2)
      }
    }
  }

  "handle anyOf validation errors (none matches)" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "anyOf": [
        |    { "type": "integer" },
        |    { "minimum": 2      }
        |  ]
        |}""".stripMargin).get

    val result: JsResult[JsValue] = validator.validate(schema)(JsNumber(1.5))
    val failure: Seq[(JsPath, Seq[ValidationError])] = result.asEither.left.get
    val failureJson = failure.toJson
    val JsDefined(subErrors) = failureJson(0) \ "errors"

    result.isError must beTrue
    failureJson(0) \ "schemaPath" must beEqualTo(JsDefined(JsString("#")))
    subErrors.as[JsObject].keys must haveSize(2)
    (subErrors.as[JsObject] \ "/anyOf/1").as[JsArray].value.head \ "schemaPath" must beEqualTo(
      JsDefined(JsString("#/anyOf/1"))
    )
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
    val result: JsResult[JsValue] = validator.validate(schema)(Json.obj("foo" -> 1.5))
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      error.toJson(0) \ "schemaPath" == JsDefined(JsString("#/properties/foo"))
    }
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
    val result = validator.validate(schema)(Json.obj("foo" -> "baz"))
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      error.toJson(0) \ "msgs" == JsDefined(JsArray(Seq(JsString("Instance does not match all schemas"))))
    }
  }

  "handle oneOf validation errors (two or more match)" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "oneOf": [
        |    {
        |      "type": "integer"
        |    },
        |    {
        |      "minimum": 2
        |    }
        |  ]
        |}""".
        stripMargin).
      get

    val result: JsResult[JsValue] = validator.validate(schema)(JsNumber(3))
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      error.toJson(0) \ "msgs" == JsDefined(JsArray(Seq(JsString("Instance matches more than one schema"))))
    }
  }

  "handle oneOf validation errors (none matches)" in {
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
        |}""".
        stripMargin).get

    val result: JsResult[JsValue] = validator.validate(schema)(JsNumber(1.14))
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      val JsDefined(obj) = error.toJson(0)
      obj \ "msgs" == JsDefined(JsArray(Seq(JsString("Instance does not match any schema"))))
    }
  }


  "report correct anyOf error path" in {

    val schemaString = """{
                         |    "definitions": {
                         |        "a": {
                         |            "properties": {
                         |                "foo": { "type": "integer" }
                         |            },
                         |            "additionalProperties": false
                         |        },
                         |        "b": {
                         |            "properties": {
                         |                "bar": { "type": "number" }
                         |            },
                         |            "additionalProperties": false
                         |        }
                         |    },
                         |    "anyOf": [
                         |        {
                         |            "$ref": "#/definitions/a"
                         |        },
                         |        {
                         |            "$ref": "#/definitions/b"
                         |        }
                         |    ]
                         |}""".stripMargin




    val instance = """{
                     |    "c": 1
                     |}""".stripMargin

    val schema = JsonSource.schemaFromString(schemaString).get
    val json = Json.parse(instance)

    val result: JsResult[JsValue] = validator.validate(schema, json)
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      val firstAnyOf = (error.toJson(0) \ "errors" \ "/anyOf/0").get.as[JsArray]
      (firstAnyOf(0) \ "schemaPath").get must beEqualTo(JsString("#/anyOf/0/definitions/a"))
    }
  }

  "reporting errors for more than one missing properties" in {
    val schema = JsonSource.schemaFromString(
      """{
        |"minProperties": 2
      }""".stripMargin).get
    val json = Json.obj()
    val result: JsResult[JsValue] = validator.validate(schema, json)
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      val msgs = (error.toJson(0) \ "msgs").get.as[JsArray]
      msgs(0).get must beEqualTo(JsString("Found 0 properties, but at least 2 properties need to be present."))
    }
  }
}
