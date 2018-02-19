package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.Assets
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.test.{PlaySpecification, WithServer}

class SchemaValidatorSpec extends PlaySpecification {

  def createApp: Application = new GuiceApplicationBuilder().routes(Assets.routes(getClass)).build()

  import Version4._

  val schema: SchemaType = JsonSource.schemaFromString(
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

  implicit val locationReads: Reads[Location] = Json.reads[Location]
  val talkReads: Reads[Talk] = Json.reads[Talk]
  implicit val locationWrites: OWrites[Location] = Json.writes[Location]
  val talkWrites: OWrites[Talk] = Json.writes[Talk]
  implicit val talkFormat: OFormat[Talk] = Json.format[Talk]

  val resourceUrl: URL = getClass.getResource("/talk.json")
  val instance: JsObject = Json.obj(
    "location" -> Json.obj(
      "name" -> "Munich"
    )
  )

  "SchemaValidator(Version4)" should {

    "validate additionalProperties schema constraint via $ref" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |  "additionalProperties": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
            |}""".stripMargin).get

        // min length 10, max length 20
        val valid = Json.obj("title" -> "This is valid")
        val invalid = Json.obj("title" -> "Too short")

        val validator = SchemaValidator(Version4)
        validator.validate(schema, valid).isSuccess must beTrue
        validator.validate(schema, invalid).isError must beTrue
      }

    "return unaltered validated array" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "array",
          |  "items": { "type": "integer" }
          |}""".stripMargin).get
      val result = SchemaValidator(Version4).validate(schema)(Json.arr(1,2,3))
      result.asOpt must beSome.which {
        case arr@JsArray(seq) => seq must haveLength(3)
      }
    }

    "validate oneOf constraint via $ref" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val valid = JsString("Valid instance")
        val invalid = JsString("Too short")

        val validator = SchemaValidator(Version4)
        validator.validate(schema, valid).isSuccess must beTrue
        validator.validate(schema, invalid).isError must beTrue
      }

    "be validated via file based resource URL" in {
      val result = SchemaValidator(Version4).validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }

    "validate via file based URL and Reads" in {
      val result: JsResult[Talk] = SchemaValidator(Version4).validate(resourceUrl, instance, talkReads)
      result.isSuccess must beTrue
      result.asOpt must beSome.which(talk => talk.location.name must beEqualTo("Munich"))
    }

    "validate via file based URL and Writes" in {
      val talk = Talk(Location("Munich"))
      val result = SchemaValidator(Version4).validate(resourceUrl, talk, talkWrites)
      result.isSuccess must beTrue
    }

    "validate via file based URL and Format" in {
      val talk = Talk(Location("Munich"))
      val result: JsResult[Talk] = SchemaValidator(Version4).validate(resourceUrl, talk)
      result.isSuccess must beTrue
      result.asOpt must beSome.which(talk => talk.location.name must beEqualTo("Munich"))
    }

    "validate with Reads" in
      new WithServer(app = createApp, port = 1234) {
        val result = SchemaValidator(Version4).validate(schema, instance, talkReads)
        result.isSuccess must beTrue
        result.asOpt must beSome.which(talk => talk.location.name must beEqualTo("Munich"))
      }

    "validate with Writes" in
      new WithServer(app = createApp, port = 1234) {
        val talk = Talk(Location("Munich"))
        val result = SchemaValidator(Version4).validate(schema, talk, talkWrites)
        result.isSuccess must beTrue
      }
  }

  "Remote ref" should {
    "validate" in new WithServer(app = createApp, port = 1234) {
      val resourceUrl: URL = new URL("http://localhost:1234/talk.json")
      val result = SchemaValidator(Version4).validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }
  }

  "report errors of wrong reads" in
    new WithServer(app = createApp, port = 1234) {

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
      val result: JsResult[Foo] = SchemaValidator(Version4).validate(schema, fooInstance, lr)
      result.asEither must beLeft.like { case error =>
        (error.toJson(0) \ "instancePath") must beEqualTo(JsDefined(JsString("/loc")))
      }
    }

  "should fail with message in case a ref can not be resolved" in {
    val schema = JsonSource.schemaFromString(
      """{ "$ref": "#/does/not/exist" }""".stripMargin).get
    val instance = JsNumber(42)
    val result = SchemaValidator(Version4).validate(schema, instance)
    result.isError must beTrue
    result.asEither must beLeft.which { _.head._2.head.message must beEqualTo(
      """Could not resolve ref #/does/not/exist."""
    )}
  }

  "validate object with arbitrary properties of type string" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "properties": {
        |    "parameters": {
        |      "patternProperties": {
        |        ".*": {"type": "string"}
        |      }
        |    }
        | }
        |}""".stripMargin).get
    val validator = SchemaValidator(Version4)

    validator.validate(schema,
      Json.obj(
        "parameters" -> Json.obj(
          "param1" -> "bar",
          "param2" -> "foo"
        )
      )
    ).isSuccess must beTrue

    validator.validate(schema,
      Json.obj(
        "parameters" -> Json.obj(
          "param1" -> 3,
          "param2" -> "foo"
        )
      )
    ).isError must beTrue
  }

  "validate object with edges property where each entry must be an object with a single property" +
    "of type string array" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "properties": {
        |    "edges": {
        |      "items": {
        |        "patternProperties": {
        |          ".*": {
        |            "type": "array",
        |            "items": { "type": "string" }
        |          }
        |        },
        |        "maxProperties": 1
        |      }
        |    }
        |  }
        |}""".stripMargin).get
    val validator = SchemaValidator(Version4)

    validator.validate(schema,
      Json.obj(
        "edges" ->
          Json.arr(
            Json.obj("A" -> Json.arr("B")),
            Json.obj("B" -> Json.arr("C", "D", "E")),
            Json.obj("D" -> Json.arr("F", "E"))
          )
      )
    ).isSuccess must beTrue

    validator.validate(schema,
      Json.obj(
        "edges" ->
          Json.arr(
            Json.obj("A" -> Json.arr(3)),
            Json.obj("B" -> Json.arr("C", "D", "E"))
          )
      )
    ).isError must beTrue

    validator.validate(schema,
      Json.obj(
        "edges" ->
          Json.arr(
            Json.obj(
              "A" -> Json.arr("C", "D", "E"),
              "B" -> 3
            )
          )
      )
    ).isError must beTrue

    validator.validate(schema,
      Json.obj(
        "edges" ->
          Json.arr(Json.obj("B" -> 3))
      )
    ).isError must beTrue
  }

  "merge properties with oneOf sub-schemas (#54)" in {
    val schema = JsonSource.schemaFromString(
      """
        |{
        |    "type" : "object",
        |    "properties" : {
        |        "name" : { "type": "string"},
        |        "name2": { "type": "string"}
        |    },
        |    "enum": [
        |        { "name":  "foo" },
        |        { "name2": "bar" }
        |    ],
        |    "oneOf" : [
        |        {
        |            "required": ["name"]
        |        },
        |        {
        |            "required": ["name2"]
        |        }
        |    ]
        |}
      """.stripMargin).get
    val validator = SchemaValidator(Version4)

    // invalid since empty object matches no schema
    validator.validate(schema)(Json.obj()) must beLike {
      case JsError(errors) => errors.head._2.head.message must beEqualTo(s"Instance does not match any schema.")
    }

    // invalid since object with both fields matches both schemas
    validator.validate(schema)(Json.obj("name" -> "foo", "name2" -> "bar")) must beLike {
      case JsError(errors) => errors.head._2.head.message must beEqualTo(s"Instance matches more than one schema.")
    }

    // valid with name field
    validator.validate(schema)(Json.obj("name" -> "foo")).isSuccess must beTrue

    // valid with name2 field
    validator.validate(schema)(Json.obj("name2" -> "bar")).isSuccess must beTrue

    // invalid because not listed in enum
    SchemaValidator(Version4).validate(schema)(Json.obj("name" -> "quux")).isError must beTrue
  }

  "verify cache hit (issue #98)" in {
    val talk = Talk(Location("Munich"))
    val validator = SchemaValidator(Version4)
    validator.validate(resourceUrl, talk)
    validator.refResolver.cache.mapping.isEmpty must beFalse
  }

  "add dependencies via addSchema and validate (#98)" in {
    val validator = SchemaValidator(Version4)
    val commonSchema = JsonSource.schemaFromString(
      """|{
         |  "definitions": {
         |    "foo": {"type": "string"}
         |  }
         |}
      """.stripMargin
    ).get
    val result = validator
      .addSchema("common-schema.json", commonSchema)
      .validate(
        JsonSource.schemaFromString(
          """|{
             |  "allOf": [
             |    {
             |      "properties": {
             |        "something": {
             |          "$ref": "common-schema.json#/definitions/foo"
             |        }
             |      }
             |    }
             |  ]
             |}""".stripMargin).get,
        Json.obj(
          "something" -> "foo"
        )
      )
    result.isSuccess must beTrue
  }

  "add dependencies via addSchema and fail (#98)" in {
    val validator = SchemaValidator(Version4)
    val commonSchema = JsonSource.schemaFromString(
      """|{
         |  "definitions": {
         |    "foo": {"type": "integer"}
         |  }
         |}
      """.stripMargin
    ).get
    val result = validator
      .addSchema("common-schema.json", commonSchema)
      .validate(
        JsonSource.schemaFromString(
          """|{
             |  "allOf": [
             |    {
             |      "properties": {
             |        "something": {
             |          "$ref": "common-schema.json#/definitions/foo"
             |        }
             |      }
             |    }
             |  ]
             |}""".stripMargin).get,
        Json.obj(
          "something" -> "foo"
        )
      )
    val errors = result.asEither.left.get
    val error = errors.toJson(0)
    error \ "errors" \ "/allOf/0" \ 0 \ "origin" must beEqualTo(JsDefined(JsString("#/allOf/0/properties/something")))
    error \ "errors" \ "/allOf/0" \ 0 \ "schemaURI" must beEqualTo(JsDefined(JsString("common-schema.json")))
    error \ "errors" \ "/allOf/0" \ 0 \ "schemaPath" must beEqualTo(JsDefined(JsString("#/definitions/foo")))

    result.isSuccess must beFalse
  }
}
