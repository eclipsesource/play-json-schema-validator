package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.{JsString, Json}

class ReferenceSpec extends Specification with JsonSpec { self =>

  implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
  import Version4._

  "validate draft4" in {
    validate("ref", "draft4")
  }

  "validate draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("ref", "draft7")
  }

  "root pointer ref - mismatch" in {
    val validator = SchemaValidator(Some(Version4))
    val recursiveSchema = JsonSource.schemaFromString(
      """{
        |  "properties": {
        |    "foo": {"$ref": "#"}
        |  },
        |  "additionalProperties": false
        |}""".stripMargin)

    val instance = Json.obj("bar" -> false)

    val result = validator.validate(recursiveSchema.get, instance)
    result.asEither must beLeft.like {
      case error => (error.toJson(0) \ "instancePath").get.as[String] == ""
    }
    result.isError must beTrue
  }

  "root pointer ref - recursive mismatch" in {
    val validator = SchemaValidator(Some(Version4))
    val recursiveSchema = JsonSource.schemaFromString(
      """{
        |  "properties": {
        |    "foo": {"$ref": "#"}
        |  },
        |  "additionalProperties": false
        |}""".stripMargin)

    val instance = Json.obj("foo" -> Json.obj("bar" -> false))
    val result   = validator.validate(recursiveSchema.get, instance)
    result.asEither must beLeft.like {
      case error => (error.toJson(0) \ "instancePath").get.as[String] == "/foo"
    }
    result.isError must beTrue
  }

  "relative pointer ref to object - mismatch" in {
    val schema = JsonSource.schemaFromString(
      """
        |{
        |  "properties": {
        |    "foo": {"type": "integer"},
        |    "bar": {"$ref": "#/properties/foo"}
        |  }
        |}
      """.stripMargin)
    val instance = Json.obj("bar" -> true)
    val result   = validator.validate(schema.get, instance)
    val errors   = result.asEither.left.get
    (errors.toJson(0) \ "instancePath").get.as[String] must beEqualTo("/bar")
    (errors.toJson(0) \ "schemaPath").get.as[String]   must beEqualTo("#/properties/foo")
    result.isError must beTrue
  }

  "relative pointer ref to array - mismatch array" in {
    val schema = JsonSource.schemaFromString(
      """
        |{
        |  "items": [
        |    {"type": "integer"},
        |    {"$ref": "#/items/0"}
        |  ]
        |}
      """.stripMargin)
    val instance = Json.arr(1, "foo")
    val result   = validator.validate(schema.get, instance)
    val errors   = result.asEither.left.get

    (errors.toJson(0) \ "instancePath").get.as[String] must beEqualTo("/1")
    (errors.toJson(0) \ "schemaPath").get.as[String]   must beEqualTo("#/items/0")
    (errors.toJson(0) \ "value").get.as[String]        must beEqualTo("foo")
    result.isError must beTrue
  }

  "nested refs - nested ref invalid" in {
    val schema = JsonSource.schemaFromString(
      """
        |{
        |  "definitions": {
        |    "a": {"type": "integer"},
        |    "b": {"$ref": "#/definitions/a"},
        |    "c": {"$ref": "#/definitions/b"}
        |  },
        |  "$ref": "#/definitions/c"
        |}
      """.stripMargin)
    val instance = JsString("a")
    val result   = validator.validate(schema.get, instance)
    val errors   = result.asEither.left.get
    (errors.toJson(0) \ "instancePath").get.as[String] must beEqualTo("")
    (errors.toJson(0) \ "schemaPath").get.as[String]   must beEqualTo("#/definitions/a")
    (errors.toJson(0) \ "value").get.as[String]        must beEqualTo("a")
    result.isError must beTrue
  }

  "remote ref, containing refs itself - remote ref invalid" in {
    val schema = JsonSource.schemaFromString(
      """{"$ref": "http://json-schema.org/draft-04/schema#"}""".stripMargin
    )
    val instance   = Json.obj("minLength" -> -1)
    val result     = validator.validate(schema.get, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "instancePath").get.as[String] must beEqualTo("/minLength")
    (firstError \ "schemaPath").get.as[String]   must beEqualTo("#/definitions/positiveIntegerDefault0")
    (firstError \ "errors").get must beEqualTo(
      Json.obj(
        "/allOf/0" -> Json.arr(
          Json.obj(
            "schemaPath" -> "#/allOf/0/definitions/positiveInteger",
            "errors" -> Json.obj(),
            "keyword" -> "minimum",
            "msgs"  -> Json.arr("-1 is smaller than required minimum value of 0."),
            "value" -> -1,
            "instancePath" -> "/minLength"
          )
        )
      )
    )
    (firstError \ "msgs").get must beEqualTo(Json.arr("Instance does not match all schemas."))
    result.isError must beTrue
  }

  "property named $ref that is not a reference - property named $ref invalid" in {
    val schema = JsonSource.schemaFromString(
      """
        |{
        |  "properties": {
        |    "$ref": {"type": "string"}
        |  }
        |}
      """.stripMargin)
    val instance   = Json.obj("$ref" -> 2)
    val result     = validator.validate(schema.get, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "schemaPath").get.as[String] must beEqualTo("#/properties/$ref")
    (firstError \ "instancePath").get.as[String] must beEqualTo("/$ref")
    result.isError must beTrue
  }
}
