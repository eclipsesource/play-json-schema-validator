package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json._

class AllOfSpec extends Specification with JsonSpec {

  "allOf draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Some(Version4))
    validate("allOf", "draft4")
  }

  "allOf draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Some(Version7))
    validate("allOf", "draft7")
  }

  val validator = SchemaValidator(Some(Version4))

  object Schemas {
    import Version4._
    val allOf = JsonSource.schemaFromString(
      """{
        |  "allOf": [
        |    {
        |      "properties": {
        |        "bar": {"type": "integer"}
        |      },
        |      "required": ["bar"]
        |    },
        |    {
        |      "properties": {
        |        "foo": {"type": "string"}
        |      },
        |      "required": ["foo"]
        |    }
        |  ]
        |}""".
        stripMargin
    ).get

    val withBaseSchema = JsonSource.schemaFromString(
      """
        |{
        |  "properties": {"bar": {"type": "integer"}},
        |  "required": ["bar"],
        |  "allOf" : [
        |    {
        |      "properties": {
        |        "foo": {"type": "string"}
        |      },
        |      "required": ["foo"]
        |    },
        |    {
        |      "properties": {
        |        "baz": {"type": "null"}
        |      },
        |      "required": ["baz"]
        |    }
        |  ]
        |}
      """.stripMargin
    ).get
  }


  "allOf - mismatch second" in {
    val instance      = Json.obj("foo" -> "baz")
    val result        = validator.validate(Schemas.allOf, instance)
    val errors        = result.asEither.left.get
    val firstError    = errors.toJson(0)
    val firstSubError = (firstError \ "errors" \ "/allOf/0").as[JsArray].value.head
    (firstSubError \ "schemaPath").get.as[String] must beEqualTo("#/allOf/0")
    (firstSubError \ "msgs").get.as[JsArray] must beEqualTo(Json.arr("Property bar missing."))
    result.isError must beTrue
  }

  "allOf - mismatch first" in {
    val instance      = Json.obj("bar" -> 2)
    val result        = validator.validate(Schemas.allOf, instance)
    val errors        = result.asEither.left.get
    val firstError    = errors.toJson(0)
    val firstSubError = (firstError \ "errors" \ "/allOf/1").as[JsArray].value.head
    (firstSubError \ "schemaPath").get.as[String] must beEqualTo("#/allOf/1")
    (firstSubError \ "msgs").get.as[JsArray] must beEqualTo(Json.arr("Property foo missing."))
    result.isError must beTrue
  }

  "allOf - wrong type" in {
    val instance      = Json.obj("foo" -> "baz", "bar" -> "quux")
    val result        = validator.validate(Schemas.allOf, instance)
    val errors        = result.asEither.left.get
    val firstError    = errors.toJson(0)
    val firstSubError = (firstError \ "errors" \ "/allOf/0").as[JsArray].value.head
    (firstSubError \ "schemaPath").get.as[String] must beEqualTo("#/allOf/0/properties/bar")
    (firstSubError \ "msgs").get.as[JsArray].value.head.as[String] must beEqualTo("Wrong type. Expected integer, was string.")
    result.isError must beTrue
  }

  "allOf with base schema - mismatch base schema" in {
    val instance = JsObject(Seq("foo" -> JsString("quux"), "baz" -> JsNull))
    val result   = validator.validate(Schemas.withBaseSchema, instance)
    val errors   = result.asEither.left.get
    (errors.toJson(0) \ "msgs").as[JsArray].head.as[String] must beEqualTo("Property bar missing.")
    result.isError must beTrue
  }

  "allOf with base schema - mismatch first allOf" in {
    val instance = JsObject(Seq("bar" -> JsNumber(2), "baz" -> JsNull))
    val result   = validator.validate(Schemas.withBaseSchema, instance)
    val errors   = result.asEither.left.get
    val firstError = errors.toJson(0)
    val firstSubError = (firstError \ "errors" \ "/allOf/0").as[JsArray].value.head
    (firstSubError \ "keyword").get.as[String] must beEqualTo("required")
    result.isError must beTrue
  }

  "allOf with base schema - mismatch second allOf" in {
    val instance = JsObject(Seq("foo" -> JsString("quux"), "bar" -> JsNumber(2)))
    val result   = validator.validate(Schemas.withBaseSchema, instance)
    val errors   = result.asEither.left.get
    val firstError = errors.toJson(0)
    val firstSubError = (firstError \ "errors" \ "/allOf/1").as[JsArray].value.head
    (firstSubError \ "keyword").get.as[String] must beEqualTo("required")
    result.isError must beTrue
  }

  "allOf with base schema - mismatch both" in {
    val instance       = JsObject(Seq("bar" -> JsNumber(2)))
    val result         = validator.validate(Schemas.withBaseSchema, instance)
    val errors         = result.asEither.left.get
    val firstError     = errors.toJson(0)
    val firstSubError  = (firstError \ "errors" \ "/allOf/0").as[JsArray].value.head
    val secondSubError = (firstError \ "errors" \ "/allOf/1").as[JsArray].value.head
    (firstSubError  \ "keyword").get.as[String] must beEqualTo("required")
    (secondSubError \ "keyword").get.as[String] must beEqualTo("required")
    result.isError must beTrue
  }
}

