package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.test.{Assets, JsonSpec}
import org.specs2.mutable.Specification
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.mvc.DefaultActionBuilder
import play.api.test.WithServer

class RefRemoteSpec extends Specification with JsonSpec { self =>

  val validator = SchemaValidator(Some(Version4))

  def createApp: Application = new GuiceApplicationBuilder()
    .appRoutes(app => {
      val Action = app.injector.instanceOf[DefaultActionBuilder]
      Assets.routes(Action)(getClass)
    })
    .build()

    "remote ref - remote ref invalid" in {
      import Version4._
      val integerJsonSchema = JsonSource.schemaFromStream(
        self.getClass.getResourceAsStream("/remotes/integer.json")
      ).get
      val schema = JsonSource.schemaFromString(
        """ { "$ref": "http://localhost:1234/remotes/integer.json" } """.stripMargin
      ).get
      val instance   = JsString("a")
      val result     = validator
        .addSchema("http://localhost:1234/remotes/integer.json", integerJsonSchema)
        .validate(schema, instance)
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "keyword").get.as[String] must beEqualTo("type")
      result.isError must beTrue
    }

    "fragment within remote ref - remote fragment invalid" in {
      import Version4._
      val resourceUrl: URL = self.getClass.getResource("/remotes/subSchemas.json")
      val subSchema = JsonSource.schemaFromUrl(resourceUrl).get
      val schema = JsonSource.schemaFromString(
        """{"$ref": "http://localhost:1234/remotes/subSchemas.json#/integer"}"""
      ).get
      val instance   = JsString("a")
      val result     = validator
        .addSchema("http://localhost:1234/remotes/subSchemas.json", subSchema)
        .validate(schema, instance)
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "schemaPath").get.as[String] must beEqualTo("#/integer")
      result.isError must beTrue
    }

    "ref within remote ref - ref within ref invalid" in {
      import Version4._
      val resourceUrl: URL = self.getClass.getResource("/remotes/subSchemas.json")
      val subSchema = JsonSource.schemaFromUrl(resourceUrl).get
      val schema = JsonSource.schemaFromString(
        """{ "$ref": "http://localhost:1234/remotes/subSchemas.json#/refToInteger" }"""
      ).get
      val instance   = JsString("a")
      val result     = validator
        .addSchema("http://localhost:1234/remotes/subSchemas.json", subSchema)
        .validate(schema, instance)
      val errors     = result.asEither.left.get
      val firstError = errors.toJson(0)
      (firstError \ "schemaPath").get.as[String] must beEqualTo("#/integer")
      result.isError must beTrue
    }

    "change resolution scope - change scope ref invalid" in new WithServer(createApp, port = 1234) {
      import Version4._
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
      result.isError must beTrue
      (firstError \ "resolutionScope").get.as[String] must beEqualTo("http://localhost:1234/folder/")
      (firstError \ "msgs").get.as[JsArray].value.head.as[String] must beEqualTo("Could not resolve ref folderInteger.json.")
    }

    "change resolution scope - change scope ref invalid" in {
      import Version4._
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

  "support canonical referencing by default in version 4" in new WithServer(createApp, port = 1234)  {
    import Version4._
    private val schema = JsonSource.schemaFromString(
      """{"$ref": "http://localhost:1234/remotes/subSchemas.json#/integer"}"""
    ).get
    val instance   = JsNumber(1)
    private val result = validator.validate(schema, instance)
    result.isSuccess must beTrue
  }

    "should not support canonical referencing by default in version 7" in new WithServer(createApp, port = 1234)  {
      import Version7._
      private val schema = JsonSource.schemaFromString(
        """{"$ref": "http://localhost:1234/remotes/subSchemas.json#/integer"}"""
      ).get
      val validator = SchemaValidator(Some(Version7))
      val instance   = JsNumber(1)
      private val result     = validator.validate(schema, instance)
      private val errors     = result.asEither.left.get
      private val firstError = errors.toJson(0)
      (firstError \ "msgs").get.as[JsArray].value.head.as[String] must beEqualTo(
        "Could not resolve ref http://localhost:1234/remotes/subSchemas.json#/integer."
      )
      result.isError must beTrue
    }

    "should support canonical referencing with option in version 7" in new WithServer(createApp, port = 1234)  {
      import Version7._
      private val schema = JsonSource.schemaFromString(
        """{"$ref": "http://localhost:1234/remotes/subSchemas.json#/integer"}"""
      ).get
      private val options = new SchemaConfigOptions  {
        override def supportsExternalReferences: Boolean = true
        override def formats: Map[String, SchemaFormat] = DefaultFormats.formats
      }
      val validator = SchemaValidator(Some(Version7(options)))
      val instance   = JsNumber(1)
      private val result     = validator.validate(schema, instance)
      result.isSuccess must beTrue
    }
}
