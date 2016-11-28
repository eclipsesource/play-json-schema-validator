package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.WithServer

class RefRemoteSpec extends Specification with JsonSpec {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }

  def createApp = new GuiceApplicationBuilder().routes(routes).build()

  "remote ref - remote ref invalid" in new WithServer(createApp, port = 1234) {
    val validator = SchemaValidator()
    val schema = JsonSource.schemaFromString(
      """ { "$ref": "http://localhost:1234/remotes/integer.json" } """.stripMargin
    ).get
    val instance   = JsString("a")
    val result     = validator.validate(schema, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "keyword").get.as[String] must beEqualTo("type")
    result.isError must beTrue
  }

  "fragment within remote ref - remote fragment invalid" in new WithServer(createApp, port = 1234)  {
    val schema = JsonSource.schemaFromString(
      """{"$ref": "http://localhost:1234/remotes/subSchemas.json#/integer"}"""
    ).get
    val instance   = JsString("a")
    val result     = validator.validate(schema, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "schemaPath").get.as[String] must beEqualTo("#/integer")
    result.isError must beTrue
  }

  "ref within remote ref - ref within ref invalid" in new WithServer(createApp, port = 1234) {
    val schema = JsonSource.schemaFromString(
      """{ "$ref": "http://localhost:1234/remotes/subSchemas.json#/refToInteger" }"""
    ).get
    val instance   = JsString("a")
    val result     = validator.validate(schema, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "schemaPath").get.as[String] must beEqualTo("#/integer")
    result.isError must beTrue
  }

  "change resolution scope - change scope ref invalid" in new WithServer(createApp, port = 1234) {
    val schema = JsonSource.schemaFromString(
      """{
        |  "id": "http://localhost:1234/",
        |  "items": {
        |    "id": "folder/",
        |    "items": {"$ref": "folderInteger.json"}
        |  }
        |}""".stripMargin
    ).get
    val instance   = Json.arr(Json.arr("a"))
    val result     = validator.validate(schema, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "resolutionScope").get.as[String] must beEqualTo("http://localhost:1234/folder/")
    (firstError \ "msgs").get.as[JsArray].value.head.as[String] must beEqualTo("Could not resolve ref folderInteger.json.")
    result.isError must beTrue
  }

  "change resolution scope - change scope ref invalid" in new WithServer(createApp, port = 1234) {
    val schema = JsonSource.schemaFromString(
      """{
        |  "id": "http://localhost:1234/remotes/",
        |  "items": {
        |    "id": "folder/",
        |    "items": {"$ref": "folderInteger.json"}
        |  }
        |}""".stripMargin
    ).get
    val instance   = Json.arr(Json.arr("a"))
    val result     = validator.validate(schema, instance)
    val errors     = result.asEither.left.get
    val firstError = errors.toJson(0)
    (firstError \ "schemaPath").get.as[String] must beEqualTo("#")
    (firstError \ "instancePath").get.as[String] must beEqualTo("/0/0")
    (firstError \ "resolutionScope").get.as[String] must beEqualTo("http://localhost:1234/remotes/folder/")
    result.isError must beTrue
  }
}
