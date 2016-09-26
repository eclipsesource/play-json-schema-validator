package com.eclipsesource.schema

import java.net.{URL, URLConnection, URLStreamHandler}

import com.eclipsesource.schema.urlhandlers.ClasspathUrlHandler
import org.specs2.mutable.Specification
import play.api.libs.json.{JsDefined, JsString, Json}

class UrlHandlerSpec extends Specification {

  "UrlHandlers" should {

    // no handler at all
    "should fail to resolve absolute references on the classpath if not handler available" in {
      val validator = SchemaValidator()
      val someJson = getClass.getResourceAsStream("/schemas/my-schema-with-protocol-ful-absolute-path.schema")
      val schema = JsonSource.schemaFromStream(someJson)
      validator.validate(schema.get, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
        .isError must beTrue
    }

    // absolute protocol-ful handler
    "should resolve absolute references on the classpath with ClasspathUrlProtocolHandler" in {
      val validator = SchemaValidator().addUrlHandler(new ClasspathUrlHandler(), ClasspathUrlHandler.Scheme)
      val someJson = getClass.getResourceAsStream("/schemas/my-schema-with-protocol-ful-absolute-path.schema")
      val schema = JsonSource.schemaFromStream(someJson)
      validator.validate(schema.get, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
        .isSuccess must beTrue
    }

    // relative protocol-ful handler with valid instance
    "should resolve protocol-ful relative references on the classpath with ClasspathUrlProtocolHandler" in {
      val validator = SchemaValidator().addRelativeUrlHandler(new ClasspathUrlHandler(), ClasspathUrlHandler.Scheme)
      val url = getClass.getResource("/schemas/my-schema-with-protocol-ful-relative-path.schema")
      validator.validate(url, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
        .isSuccess must beTrue
    }

    // relative protocol-ful handler with invalid instance
    "should resolve protocol-ful relative references on the classpath with ClasspathUrlProtocolHandler (invalid instance)" in {
      val validator = SchemaValidator().addRelativeUrlHandler(new ClasspathUrlHandler(), ClasspathUrlHandler.Scheme)
      val url = getClass.getResource("/schemas/my-schema-with-protocol-ful-relative-path.schema")
      val result = validator.validate(url, Json.obj("foo" -> Json.obj("bar" -> 42)))
      result.asEither must beLeft.like { case error =>
        error.toJson(0) \ "msgs" == JsDefined(Json.arr(JsString("Expected string, was number.")))
      }
    }

    // no relative protocol-ful handler registered, should fail
    "should resolve protocol-ful relative references on the classpath with ClasspathUrlProtocolHandler (invalid instance)" in {
      val validator = SchemaValidator()
      val url = getClass.getResource("/schemas/my-schema-with-protocol-ful-relative-path.schema")
      validator.validate(url, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
         .isError must beTrue
    }

    // if no URL handlers have been registered the default ones will be used
    "should resolve protocol-less relative references on the classpath via default behaviour (valid instance)" in {
      val validator = SchemaValidator()
      val url = getClass.getResource("/schemas/my-schema-with-protocol-less-relative-path.schema")
      validator.validate(url, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
        .isSuccess must beTrue
    }

    "should resolve protocol-less relative references on the classpath with via default behaviour (invalid instance)" in {
      val validator = SchemaValidator()
      val url = getClass.getResource("/schemas/my-schema-with-protocol-less-relative-path.schema")
      validator.validate(url, Json.obj("foo" -> Json.obj("bar" -> 3)))
        .isError must beTrue
    }

    // use custom URLStreamHandler in order to override default behaviour
    // this URL handler just returns the foo.schema for any given url
    class MyUrlHandler extends URLStreamHandler {
      override def openConnection(url: URL): URLConnection = {
        getClass.getResource("/issue-65/schemas/foo.schema").openConnection()
      }
    }

    "should resolve protocol-less relative references on the classpath with custom relative URL handler (valid instance)" in {
      val validator = SchemaValidator().addRelativeUrlHandler(new MyUrlHandler)
      val url = getClass.getResource("/issue-65/schemas/my-schema-with-protocol-less-relative-path.schema")
      val result = validator.validate(url, Json.obj("foo" -> Json.obj("bar" -> "Munich")))
      result.isSuccess must beTrue
    }

    "should resolve protocol-less relative references on the classpath with custom relative URL handler (invalid instance)" in {
      val validator = SchemaValidator().addRelativeUrlHandler(new MyUrlHandler)
      val url = getClass.getResource("/issue-65/schemas/my-schema-with-protocol-less-relative-path.schema")
      val invalidResult = validator.validate(url, Json.obj("quux" -> Json.obj("bar" -> 3)))
      invalidResult.isError must beTrue
    }

    "should fail resolving protocol-less relative references on the classpath if no relative URL handler registered" in {
      val validator = SchemaValidator()
      val url = getClass.getResource("/issue-65/schemas/my-schema-with-protocol-less-relative-path.schema")
      validator.validate(url, Json.obj("quux" -> "Munich"))
        .isError must beTrue
    }
  }
}
