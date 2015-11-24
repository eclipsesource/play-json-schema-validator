package com.eclipsesource.schema

import java.net.URL

import controllers.Assets
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.{FakeApplication, PlaySpecification, WithServer}
import play.api.libs.json.Reads._
import play.api.libs.json._

class SchemaValidatorSpec extends PlaySpecification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }

  case class Location(name: String)
  case class Talk(location: Location)

  implicit val locationReads = Json.reads[Location]
  implicit val talkReads = Json.reads[Talk]

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

        val valid = Json.obj("title" -> "This is a valid instance")
        val invalid = Json.obj("title" -> "Too short")

        SchemaValidator.validate(schema, valid).isSuccess must beTrue
        SchemaValidator.validate(schema, invalid).isFailure must beTrue
      }


    "validate oneOf constraint via $ref" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val valid = JsString("Valid instance")
        val invalid = JsString("Too short")

        SchemaValidator.validate(schema, valid).isSuccess must beTrue
        SchemaValidator.validate(schema, invalid).isFailure must beTrue
      }

    "be validated via file based resource URL" in {
      val result = SchemaValidator.validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }


    "validate with implicit Reads" in {
      val result = SchemaValidator.validate(resourceUrl, instance, talkReads)
      result.isSuccess must beTrue
      result.get.location.name must beEqualTo("Munich")
    }
  }

  "Remote ref" should {
    "validate" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val resourceUrl: URL = new URL("http://localhost:1234/talk.json")
      val result = SchemaValidator.validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }

  }

}
