package com.eclipsesource.schema

import com.eclipsesource.schema.internal.Keywords
import org.specs2.mutable.Specification
import play.api.libs.json._

class ErrorReportingSpec extends Specification {

  case class Post(id: Long, title: String, body: String)

  val validator = SchemaValidator()

  "Validator" should {

    "report error for wrong type" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "foo": { "type": "integer" }
          |  }
          |}""".stripMargin).get
      val result = validator.validate(schema)(Json.obj("foo" -> "bar"))
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "msgs").get.as[JsArray].value.head ==
        JsString("Wrong type. Expected integer, was string.")
      }
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "keyword").get ==
        JsString(Keywords.Any.Type)
      }
    }

    "report error with instance path" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "foo": { "type": "integer" }
          |  }
          |}""".stripMargin).get
      val result = validator.validate(schema)(Json.obj("foo" -> "bar"))
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "instancePath").get ==
        JsString("/foo")
      }
    }

    "report 'refs' error with schema path" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "definitions": {
          |    "num": { "type": "number" }
          |  },
          |  "properties": {
          |    "foo": { "$ref": "#/definitions/num" }
          |  }
          |}
        """.stripMargin).get
      val result = validator.validate(schema)(Json.obj("foo" -> "bar"))
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "instancePath").get ==
        JsString("/foo")
      }
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "schemaPath").get ==
        JsString("#/definitions/num")
      }
    }

    "report multiple errors if additionalProperties set" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "foo": {},
          |    "bar": {}
          |  },
          |  "additionalProperties": false
          |}
        """.stripMargin).get
      val data = Json.obj("foo" -> 1, "bar" -> 2)
      val invalidData = Json.obj("foo" -> 1, "bar" -> 2, "baz" -> 3, "quux" -> 4)
      validator.validate(schema)(data).isSuccess must beTrue

      val result = validator.validate(schema)(invalidData)
      result.asEither must beLeft.like { case error =>
        (error.toJson(0) \ "instancePath").get == JsString("")
      }
    }

    "report errors for additionalProperties with object schema" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |     "foo": { "type": "integer" },
          |     "bar": { "type": "integer" }
          |  },
          |  "additionalProperties": {
          |    "type": "object",
          |    "properties": {
          |      "quux": { "type": "string" }
          |    }
          |  }
          |}""".stripMargin).get

      val invalidData = Json.obj(
        "foo" -> 1,
        "bar" -> 2,
        "baz" -> Json.obj(
          "quux" -> 3,
          "boo" -> Json.obj(
            "quux" -> 4
          )
        )
      )
      val result = validator.validate(schema)(invalidData)
      result.isError must beTrue
      val errors = result.asEither.left.get.toJson
      (errors(0) \ "instancePath") must beEqualTo(JsDefined(JsString("/baz/quux")))
      (errors(0) \ "schemaPath") must beEqualTo(JsDefined(JsString("#/additionalProperties/properties/quux")))
    }

    "report error for nested proprety with additionalProperties set" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |     "foo": { "type": "integer" },
          |     "bar": { "type": "integer" },
          |     "fuz": {
          |       "additionalProperties": {
          |         "type": "object",
          |         "properties": {
          |           "quux": { "type": "string" }
          |         }
          |       }
          |     }
          |  }
          |}""".stripMargin).get
      val invalidData = Json.obj(
        "foo" -> 1,
        "bar" -> 2,
        "fuz" -> Json.obj(
          "baz" -> Json.obj(
            "quux" -> 3,
            "boo" -> Json.obj(
              "quux" -> 4
            )
          )
        )
      )
      val result = validator.validate(schema)(invalidData)
      result.isError must beTrue
      val errors = result.asEither.left.get.toJson
      (errors(0) \ "instancePath") must beEqualTo(JsDefined(JsString("/fuz/baz/quux")))
      (errors(0) \ "schemaPath") must beEqualTo(JsDefined(JsString("#/properties/fuz/additionalProperties/properties/quux")))
    }

    "report correct schema path for additionalItems" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "properties": {
          |    "foo": {
          |      "type": "array",
          |      "items": [ { "type": "integer" }, { "type": "integer" } ],
          |      "additionalItems": false
          |    }
          |  }
          |}
        """.stripMargin).get
      val invalidData = Json.obj("foo" -> Json.arr(1, 2, 3))
      val result = validator.validate(schema)(invalidData)
      result.asEither must beLeft.like { case error =>
        error.toJson(0) \ "instancePath" == JsDefined(JsString("/foo"))
      }
    }

    "handle multiple errors for same property" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "id":    { "type": "integer" },
          |    "title": { "type": "string", "minLength": 3, "pattern": "^[A-Z].*" }
          |  }
          |}""".stripMargin).get
      val result = validator.validate(schema)(Json.obj("title" -> "a"))
      result.isError must beTrue
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "msgs").get.as[JsArray].value.size == 2 }
    }

    "handle multiple errors for different properties" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "id":    { "type": "integer" },
          |    "title": { "type": "string", "minLength": 3 },
          |    "foo"  : { "type": "string", "pattern": "^[A-Z].*" }
          |  }
          |}""".stripMargin).get
      val result = validator.validate(schema)(Json.obj("title" -> "a", "foo" -> "a"))
      result.asEither must beLeft.like { case error =>
        error.toJson.value must haveLength(2)
      }
    }

    "handle multiple required errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "id":    { "type": "integer" },
          |    "foo":   { "type": "string"  },
          |    "bar":   { "type": "string"  }
          |  },
          |  "required": ["foo", "bar"]
          |}""".stripMargin).get
      val result = validator.validate(schema)(Json.obj())
      result.isError must beTrue
      result.asEither must beLeft.like { case error => error.toJson.as[JsArray].value.size == 2 }
    }

    "handle nested multiple required errors" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "properties": {
          |    "id":    { "type": "integer" },
          |    "foo":   { "type": "string"  },
          |    "bar":   {
          |      "type": "object",
          |      "properties": {
          |        "quux": { "type": "integer" }
          |      },
          |      "required": ["quux"]
          |    }
          |  },
          |  "required": ["foo", "bar"]
          |}""".stripMargin).get
      val result = validator.validate(schema)(Json.obj("bar" -> Json.obj()))
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        error.toJson.value must haveLength(2)
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

      val result = validator.validate(schema)(JsNumber(1.5))
      result.isError must beTrue
      val Left(failures) =  result.asEither.left.map(_.toJson)
      val errors = (failures(0) \ "errors").get

      (failures(0) \ "schemaPath") must beEqualTo(JsDefined(JsString("#")))
      errors.as[JsObject].keys must haveSize(2)
      (errors \ "/anyOf/1").as[JsArray].value.head \ "schemaPath" must beEqualTo(
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
      val result = validator.validate(schema)(Json.obj("foo" -> 1.5))
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        (error.toJson(0) \ "schemaPath") == JsDefined(JsString("#/properties/foo"))
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
      val result     = validator.validate(schema)(Json.obj("foo" -> "baz"))
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].head.as[String] must beEqualTo("Instance does not match all schemas.")
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
          |}""".stripMargin).get

      val result     = validator.validate(schema)(JsNumber(3))
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].head.as[String] must beEqualTo("Instance matches more than one schema.")
    }

    "handle oneOf validation errors (none matches)" in {
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
          stripMargin).get

      val result = validator.validate(schema)(JsNumber(1.14))
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].head.as[String] must beEqualTo("Instance does not match any schema.")
    }

    "report correct anyOf error path" in {
      val schema = JsonSource.schemaFromString("""
                                                 |{
                                                 |  "definitions": {
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
                                                 |}""".stripMargin).get

      val instance = Json.obj("c" -> 1)
      val result = validator.validate(schema)(instance)
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        val firstAnyOf = (error.toJson(0) \ "errors" \ "/anyOf/0").get.as[JsArray]
        (firstAnyOf(0) \ "schemaPath").get must beEqualTo(JsString("#/anyOf/0/definitions/a"))
      }
    }

    "reporting errors for more than one missing property" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"minProperties": 2
          }""".stripMargin).get
      val invalidData = Json.obj()
      val result = validator.validate(schema, invalidData)
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].head.as[String] must beEqualTo("Found 0 properties, but a minimum of 2 is required.")
    }

    // TODO: test with fge
    "reporting errors with correct resolution scope " in {
      val schema = JsonSource.schemaFromString(
        """{
          |"id": "foo",
          |"minProperties": 2
      }""".stripMargin).get
      val invalidData = Json.obj()
      val result = validator.validate(schema, invalidData)
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        (error.toJson(0) \ "resolutionScope") == JsDefined(JsString("foo"))
      }
    }


    "report errors for items" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "id": "schema1",
          |  "type": "array",
          |  "items": {
          |    "type": "integer",
          |    "minimum": 10
          |  }
          |}
        """.stripMargin).get
      val data = Json.arr(10, 11, 12)
      val invalidData = Json.arr(1, 10)
      // TODO: report index
      // val invalidData2 = Json.arr(10, 9, 11, 8 ,12)
      val result = validator.validate(schema)(invalidData)
      result.asEither must beLeft.like { case error =>
        (error.toJson(0) \ "instancePath").get == JsString("/0")
      }
    }

    "report address sample errors" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "$schema": "http://json-schema.org/draft-04/schema#",
          |
          |  "definitions": {
          |    "address": {
          |      "type": "object",
          |      "properties": {
          |        "street_address": { "type": "string" },
          |        "city":           { "type": "string" },
          |        "state":          { "type": "string" }
          |      },
          |      "required": ["street_address", "city", "state"]
          |    }
          |  },
          |
          |  "type": "object",
          |
          |  "properties": {
          |    "billing_address": { "$ref": "#/definitions/address" },
          |    "shipping_address": { "$ref": "#/definitions/address" }
          |  }
          |}
        """.stripMargin).get
      val invalidData = Json.parse(
        """
          |{
          |  "shipping_address": {
          |    "street_address": "1600 Pennsylvania Avenue NW",
          |    "city": "Washington",
          |    "state": "DC"
          |  },
          |  "billing_address": {
          |    "street_address": "1st Street SE",
          |    "city": 3,
          |    "state": "DC"
          |  }
          |}
        """.stripMargin)
      val result = validator.validate(schema)(invalidData)
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        (error.toJson(0) \ "instancePath") == JsDefined(JsString("/billing_address/city"))
      }
    }

    "report match error" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "pattern": "^[A-Za-z0-9]+$"
          |}
        """.stripMargin).get
      val result = validator.validate(schema)(JsString("asdfasdf*"))
      result.isError must beTrue
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].head.as[String] must beEqualTo("'asdfasdf*' does not match pattern '^[A-Za-z0-9]+$'.")
    }

    "report invalid pattern error" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "pattern": "***"
          |}
        """.stripMargin).get
      val result = validator.validate(schema)(JsString("asdfasdf*"))
      result.isError must beTrue
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].head.as[String] must beEqualTo("Invalid pattern '***'.")
    }
  }
}
