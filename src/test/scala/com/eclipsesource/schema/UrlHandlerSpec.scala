package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.urlhandlers.ClasspathUrlHandler
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class UrlHandlerSpec extends Specification with ErrorHelper { self =>

  "UrlHandlers" should {

    import Version4._

    val clazz = this.getClass

    // no handler at all
    "should fail to resolve absolute references on the classpath if not handler available" in {
      val validator = SchemaValidator(Some(Version4))
      val someJson = clazz.getResourceAsStream("/schemas/my-schema-with-protocol-ful-absolute-path.schema")
      val schema = JsonSource.schemaFromStream(someJson)
      validator.validate(schema.get, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
        .isError must beTrue
    }

    // absolute protocol-ful handler
    "should resolve absolute references on the classpath with ClasspathUrlProtocolHandler" in {
      val validator = SchemaValidator(Some(Version4)).addUrlHandler(new ClasspathUrlHandler(), ClasspathUrlHandler.Scheme)
      val someJson = clazz.getResourceAsStream("/schemas/my-schema-with-protocol-ful-absolute-path.schema")
      val schema = JsonSource.schemaFromStream(someJson)
      val result = validator.validate(schema.get, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
      result.isSuccess must beTrue
    }

    "should resolve absolute references on classpath with ClasspathUrlProtocolHandler (version 7)" in {
      import Version7._
      val validator = SchemaValidator(Some(Version7(new SchemaConfigOptions {
        override def formats: Map[String, SchemaFormat] = DefaultFormats.formats
        override def supportsExternalReferences: Boolean = true
      }))).addUrlHandler(new ClasspathUrlHandler(), ClasspathUrlHandler.Scheme)
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "object",
          |  "properties": {
          |     "schema": {
          |        "$ref": "classpath:///refs/json-schema-draft-07.json"
          |     }
          |  }
          |}
        """.stripMargin)
      val result = validator.validate(schema.get, Json.obj("schema" -> Json.obj()))
      result.isSuccess must beTrue
    }

    "should resolve relative references on classpath (valid instance)" in {
      val validator = SchemaValidator(Some(Version4))
      val url = clazz.getResource("/schemas/my-schema-with-protocol-less-relative-path.schema")
      validator.validate(url)(Json.obj("foo" -> Json.obj("bar" -> "Munich")))
        .isSuccess must beTrue
    }

    "should resolve relative references on the classpath (invalid instance)" in {
      val validator = SchemaValidator(Some(Version4))
      val url = clazz.getResource("/schemas/my-schema-with-protocol-less-relative-path.schema")
      val res = validator.validate(url)(Json.obj("foo" -> Json.obj("bar" -> 3)))
      firstErrorOf(res) must beEqualTo("Wrong type. Expected string, was number.")
    }
  }
}
