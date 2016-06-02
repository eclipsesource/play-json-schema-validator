package com.eclipsesource.schema

import java.net.{URL, URLConnection, URLStreamHandler}

import com.eclipsesource.schema.internal.url.ClasspathUrlResolver
import controllers.Assets
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.{FakeApplication, PlaySpecification, WithServer}

class SchemaValidatorSpec extends PlaySpecification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }

  val schema = JsonSource.schemaFromString(
    """{
      |  "type": "object",
      |  "properties": {
      |    "title": {
      |      "type": "string",
      |      "minLength": 10,
      |      "maxLength": 20
      |    },
      |    "speaker": {
      |      "type": "string",
      |      "pattern": "(Mr.|Mrs.)?[A-Za-z ]+"
      |    },
      |    "location": { "$ref": "http://localhost:1234/location.json" }
      |  }
      |}""".stripMargin).get

  case class Location(name: String)
  case class Talk(location: Location)

  implicit val locationReads = Json.reads[Location]
  val talkReads = Json.reads[Talk]
  implicit val locationWrites = Json.writes[Location]
  val talkWrites = Json.writes[Talk]
  implicit val talkFormat = Json.format[Talk]

  val resourceUrl: URL = getClass.getResource("/talk.json")
  val instance = Json.obj(
    "location" -> Json.obj(
      "name" -> "Munich"
    )
  )

  "SchemaValidator" should {

    "validate additionalProperties schema constraint via $ref" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |  "additionalProperties": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
            |}""".stripMargin).get

        // min length 10, max length 20
        val valid = Json.obj("title" -> "This is valid")
        val invalid = Json.obj("title" -> "Too short")

        val validator = SchemaValidator()
        validator.validate(schema, valid).isSuccess must beTrue
        validator.validate(schema, invalid).isError must beTrue
      }

    "return unaltered validated array" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "array",
          |  "items": { "type": "integer" }
          |}""".stripMargin).get
      val result = SchemaValidator().validate(schema)(Json.arr(1,2,3))
      result.asOpt must beSome.which {
        case arr@JsArray(seq) => seq must haveLength(3)
      }
    }

    "validate oneOf constraint via $ref" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val valid = JsString("Valid instance")
        val invalid = JsString("Too short")

        val validator = SchemaValidator()
        validator.validate(schema, valid).isSuccess must beTrue
        validator.validate(schema, invalid).isError must beTrue
      }

    "be validated via file based resource URL" in {
      val result = SchemaValidator().validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }

    "validate via file based URL and Reads" in {
      val result: JsResult[Talk] = SchemaValidator().validate(resourceUrl, instance, talkReads)
      result.isSuccess must beTrue
      result.asOpt must beSome.which(talk => talk.location.name must beEqualTo("Munich"))
    }

    "validate via file based URL and Writes" in {
      val talk = Talk(Location("Munich"))
      val result = SchemaValidator().validate(resourceUrl, talk, talkWrites)
      result.isSuccess must beTrue
    }

    "validate via file based URL and Format" in {
      val talk = Talk(Location("Munich"))
      val result: JsResult[Talk] = SchemaValidator().validate(resourceUrl, talk)
      result.isSuccess must beTrue
      result.asOpt must beSome.which(talk => talk.location.name must beEqualTo("Munich"))
    }

    "validate with Reads" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
        val result = SchemaValidator().validate(schema, instance, talkReads)
        result.isSuccess must beTrue
        result.asOpt must beSome.which(talk => talk.location.name must beEqualTo("Munich"))
      }

    "validate with Writes" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val talk = Talk(Location("Munich"))
        val result = SchemaValidator().validate(schema, talk, talkWrites)
        result.isSuccess must beTrue
      }
  }

  "Remote ref" should {
    "validate" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val resourceUrl: URL = new URL("http://localhost:1234/talk.json")
      val result = SchemaValidator().validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }
  }

  "report errors of wrong reads" in
    new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

      case class Foo(location: Location, title: String)

      val lr: Reads[Foo] = (
        (JsPath \ "loc").read[Location] and
          (JsPath \ "title").read[String]
        ) (Foo.apply _)

      val fooInstance = Json.obj(
        "location" -> Json.obj(
          "name" -> "Munich"
        ),
        "title" -> "Some title"
      )
      val result: JsResult[Foo] = SchemaValidator().validate(schema, fooInstance, lr)
      result.asEither must beLeft.like { case error =>
        (error.toJson(0).get \ "instancePath") must beEqualTo(JsDefined(JsString("/loc")))
      }
    }

  "should resolve references on the classpath via UrlResolver" in {
    val validator = SchemaValidator().addUrlResolver(new ClasspathUrlResolver)
    // simple.json references location.json within JAR
    val simpleJson = getClass.getResourceAsStream("/simple.json")
    val schema = JsonSource.schemaFromStream(simpleJson)
    val result = validator.validate(schema.get, Json.obj("location" -> Json.obj("name" -> "Munich")))
    result.isSuccess must beTrue
  }

  "should resolve references on the classpath via UrlHandler" in {
    val validator = SchemaValidator()
    validator.refResolver.addUrlHandler("classpath", new URLStreamHandler {
      override def openConnection(url: URL): URLConnection = {
        getClass.getResource(url.getPath).openConnection()
      }
    })
    // simple.json references location.json within JAR
    val simpleJson = getClass.getResourceAsStream("/simple.json")
    val schema = JsonSource.schemaFromStream(simpleJson)
    val result = validator.validate(schema.get, Json.obj("location" -> Json.obj("name" -> "Munich")))
    result.isSuccess must beTrue
  }

  "should resolve references on the classpath via UrlHandler" in {
    val schema = JsonSource.schemaFromString(
      """{ "$ref": "#/does/not/exist" }""".stripMargin).get
    val instance = JsNumber(42)
    val result = SchemaValidator().validate(schema, instance)
    result.isError must beTrue
    result.asEither must beLeft.which { _.head._2.head.message must beEqualTo(
      """Could not resolve ref #/does/not/exist"""
    )}
  }
}
