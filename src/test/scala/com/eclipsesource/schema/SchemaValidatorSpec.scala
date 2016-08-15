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
    val validator = SchemaValidator().addUrlResolver(ClasspathUrlResolver())
    // some.json references location.json within JAR
    val someJson = getClass.getResourceAsStream("/some.json")
    val schema = JsonSource.schemaFromStream(someJson)
    val result = validator.validate(schema.get, Json.obj("location" -> Json.obj("name" -> "Munich")))
    result.isSuccess must beTrue
  }

  "should resolve references on the classpath via UrlResolver and custom protocols (#65)" in {
    val validator = SchemaValidator()
      .addUrlResolver(ClasspathUrlResolver())
      .shouldResolveRelativeRefsWithCustomProtocols(true)
    // some.json references location.json within JAR
    val someJson = getClass.getResourceAsStream("/some-issue-65.json")
    val schema = JsonSource.schemaFromStream(someJson)
    val result = validator.validate(schema.get, Json.obj("location" -> Json.obj("name" -> "Munich")))
    result.isSuccess must beTrue
  }

  "should resolve references on the classpath via UrlHandler" in {
    val validator = SchemaValidator().addUrlHandler("classpath", new URLStreamHandler {
      override def openConnection(url: URL): URLConnection = {
        getClass.getResource(url.getPath).openConnection()
      }
    })
    // some.json references another JSON within same JAR
    val someJson = getClass.getResourceAsStream("/some.json")
    val schema = JsonSource.schemaFromStream(someJson)
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
    val validator = SchemaValidator()

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
    val validator = SchemaValidator()

    val res1 = validator.validate(schema,
      Json.obj(
        "edges" ->
          Json.arr(
            Json.obj("A" -> Json.arr("B")),
            Json.obj("B" -> Json.arr("C", "D", "E")),
            Json.obj("D" -> Json.arr("F", "E"))
          )
      )
    )

    res1.isSuccess must beTrue

    val res2 = validator.validate(schema,
      Json.obj(
        "edges" ->
          Json.arr(
            Json.obj("A" -> Json.arr(3)),
            Json.obj("B" -> Json.arr("C", "D", "E"))
          )
      )
    )

    res2.isError must beTrue

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
    val validator = SchemaValidator()

    // invalid since empty object matches no schema
    validator.validate(schema)(Json.obj()) must beLike {
      case JsError(errors) => errors.head._2.head.message must beEqualTo(s"Instance does not match any schema")
    }

    // invalid since object with both fields matches both schemas
    validator.validate(schema)(Json.obj("name" -> "foo", "name2" -> "bar")) must beLike {
      case JsError(errors) => errors.head._2.head.message must beEqualTo(s"Instance matches more than one schema")
    }

    // valid with name field
    validator.validate(schema)(Json.obj("name" -> "foo")).isSuccess must beTrue

    // valid with name2 field
    validator.validate(schema)(Json.obj("name2" -> "bar")).isSuccess must beTrue



    // invalid because not listed in enum
    val result = SchemaValidator().validate(schema)(Json.obj("name" -> "quux"))
    result.isError must beTrue
  }
}
