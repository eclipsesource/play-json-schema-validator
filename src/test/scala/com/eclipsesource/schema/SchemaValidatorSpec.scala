package com.eclipsesource.schema

import java.net.{HttpURLConnection, URL}

import com.eclipsesource.schema.internal.Context
import com.eclipsesource.schema.internal.RefResolver
import com.eclipsesource.schema.internal.constraints.Constraints.ObjectConstraints
import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.libs.json.{JsObject, JsBoolean, JsString, Json}
import play.api.mvc.Handler
import play.api.data.mapping.Path
import play.api.test.{FakeApplication, WithServer, PlaySpecification}


class SchemaValidatorSpec extends PlaySpecification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }

  val resourceUrl: URL = getClass.getResource("/talk.json")
  val instance = Json.obj(
    "location" -> Json.obj(
      "name" -> "Munich"
    )
  )

  "Schema" should {

    "be validated via file based resource URL" in {
      println(resourceUrl)
      val result = SchemaValidator.validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }
  }

  "Remote ref" should {
    "validate" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val resourceUrl: URL = new URL("http://localhost:1234/talk.json")
      val result = SchemaValidator.validate(resourceUrl, instance)
      result.isSuccess must beTrue
    }

    "be resolvable in additionalProps" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234)  {
      val schema = JsonSource.schemaFromString(
        """{
          |"additionalProperties": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
          |}""".stripMargin).get
      val instance = Json.obj("title" -> "JSON schema ftw")
      val instance2 = Json.obj("title" -> "JSONw")
      val result = SchemaValidator.validate(schema, instance)
      val result2 = SchemaValidator.validate(schema, instance2)
      result.isSuccess must beTrue
      result2.isSuccess must beFalse
    }

    "should resolve oneOf " in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val instance = JsString("JSON schema ftw")
      val instance2 = JsString("JSONwasdfffffffffffffffffffffffffffffffffffffffff")
      val context = Context(Path, Path, schema, Set.empty)

      println(RefResolver.replaceRefs(context)(schema).prettyPrint)
      val result = SchemaValidator.validate(schema, instance)
      val result2 = SchemaValidator.validate(schema, instance2)

      result.isSuccess must beTrue
      result2.isSuccess must beFalse
    }


    /// resolve

    "should resolve anyOf constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get
      val context = Context(Path, Path, schema, Set.empty)

      println(RefResolver.replaceRefs(context)(schema))

      val resolved = RefResolver.resolveRef("#/anyOf", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj(
          "items" -> Json.arr(
            Json.obj(
              "type" -> "string",
              "minLength" -> 10,
              "maxLength" -> 20
            )
          )
        )
      )
    }

    "should resolve oneOf constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get
      val context = Context(Path, Path, schema, Set.empty)

      val resolved = RefResolver.resolveRef("oneOf", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj(
          "items" -> Json.arr(
            Json.obj(
              "type" -> "string",
              "minLength" -> 10,
              "maxLength" -> 20
            )
          )
        )
      )
    }

    "should resolve allOf constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"allOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get
      val context = Context(Path, Path, schema, Set.empty)

      val resolved = RefResolver.resolveRef("#/allOf", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj(
          "items" -> Json.arr(
            Json.obj(
              "type" -> "string",
              "minLength" -> 10,
              "maxLength" -> 20
            )
          )
        )
      )
    }

    "should resolve definitions constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"definitions": {
          |  "foo": { "type": "string" }
          |}
          |}""".stripMargin).get
      val context = Context(Path, Path, schema, Set.empty)

      val resolved = RefResolver.resolveRef("#/definitions/foo", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj(
          "type" -> "string"
        )
      )
    }

    "should resolve definitions constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"minLength": 10,
          |"oneOf": [{
          |  "foo": { "type": "string" }
          |}]
          |}""".stripMargin).get
      val context = Context(Path, Path, schema, Set.empty)

      val resolved = RefResolver.resolveRef("#/oneOf/0/foo", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj(
          "type" -> "string"
        )
      )
    }

    "should resolve dependencies constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val s = JsonSource.schemaFromString(
        """{
          |"dependencies": {
          |    "a": "b",
          |    "c": ["d", "e"]
          |  }
          |}""".stripMargin)

      println(">>> "+ s)
      val schema = s.get
      val context = Context(Path, Path, schema, Set.empty)
      val resolved = RefResolver.resolveRef("#/dependencies/c/1", context)

      Json.toJson(resolved.get) must beEqualTo(JsString("e"))
    }

    "should resolve patternProperties constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val s = JsonSource.schemaFromString(
        """{
          |"patternProperties": {
          |        "^(/[^/]+)+$": {}
          |}
          |}""".stripMargin)

      val schema = s.get
      val context = Context(Path, Path, schema, Set.empty)
      println(Json.prettyPrint(Json.toJson(schema)))

      val resolved = RefResolver.resolveRef("#/patternProperties", context)
      println(resolved)
      Json.toJson(resolved.get) must beAnInstanceOf[JsObject]
    }

    "should resolve additionalProperties constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val s = JsonSource.schemaFromString(
        """{
          |"additionalProperties": false
          |}""".stripMargin)

      val schema = s.get
      val context = Context(Path, Path, schema, Set.empty)
      println(Json.prettyPrint(Json.toJson(schema)))

      val resolved = RefResolver.resolveRef("#/additionalProperties", context)
      println(resolved)
      Json.toJson(resolved.get) must beEqualTo(JsBoolean(false))
    }

    "should resolve additionalProperties constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val s = JsonSource.schemaFromString(
        """{
          |"type": "boolean",
          |"definitions": {
          |  "foo": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
          |}
          |}""".stripMargin)

      val schema = s.get
      val context = Context(Path, Path, schema, Set.empty)
      println(Json.prettyPrint(Json.toJson(schema)))

      val resolved = RefResolver.resolveRef("#/definitions/foo", context)
      println(resolved)
      Json.toJson(resolved.get) must beEqualTo(Json.obj("type" -> "string", "minLength" -> 10, "maxLength" -> 20))
    }
  }


}
