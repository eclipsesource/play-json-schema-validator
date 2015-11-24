package com.eclipsesource.schema.internal.resolvable

import com.eclipsesource.schema.internal.{GlobalContextCache, RefResolver, Context}
import com.eclipsesource.schema.{SchemaValue, SchemaValidator, JsonSource}
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.data.mapping.Path
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.{FakeApplication, WithServer}

class ResolveSpec extends Specification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }


  "RefResolver" should {


    "normalize path" in {
      // TODO reenable tests
      //      RefResolver.normalize("#foo", "http://x.y.z/rootschema.json#") must beEqualTo("http://x.y.z/rootschema.json#foo")
      //      RefResolver.normalize("otherschema.json", "http://x.y.z/rootschema.json#") must beEqualTo("http://x.y.z/otherschema.json#")
      //      RefResolver.normalize("#bar", "http://x.y.z/otherschema.json#") must beEqualTo("http://x.y.z/otherschema.json#bar")
      //      RefResolver.normalize("t/inner.json#a", "http://x.y.z/otherschema.json#") must beEqualTo("http://x.y.z/t/inner.json#a")
      true must beTrue
    }

    "resolve array constraints" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "items": {
          |    "type": "integer"
          |  },
          |  "minItems": 42
          |}""".stripMargin).get
      RefResolver.resolve("#/minItems", Context(schema)) must beSome(SchemaValue(JsNumber(42)))
    }

    "resolve anyOf constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val context = Context(schema)

        val resolved = RefResolver.resolve("#/anyOf", context)
        Json.toJson(RefResolver.resolveAll(context)(resolved.get)) must beEqualTo(Json.obj(
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

    "resolve oneOf constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val context = Context(schema)
      val resolved = RefResolver.resolve("oneOf", context)

      Json.toJson(RefResolver.resolveAll(context)(resolved.get)) must beEqualTo(
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

    "resolve allOf constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

      val schema = JsonSource.schemaFromString(
        """{
          |"allOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".
          stripMargin).get
      val context = Context(schema)

      val resolved = RefResolver.resolve("#/allOf", context)
      Json.toJson(RefResolver.resolveAll(context)(resolved.get)) must beEqualTo(
        Json.obj(
          "items" -> Json.arr(
            Json.obj
            (
              "type" ->
                "string",
              "minLength" -> 10,
              "maxLength" -> 20
            )
          )
        )
      )
    }

    "resolve definitions constraint" in new
        WithServer(app =
          new

              FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"definitions": {
          |  "foo": { "type": "string" }
          |}
          |}""".stripMargin).get

      val context = Context(schema)
      val resolved = RefResolver.resolve("#/definitions/foo", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj("type" -> "string")
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

      val context = Context(schema)
      val resolved = RefResolver.resolve("#/oneOf/0/foo", context)
      Json.toJson(resolved.get) must beEqualTo(
        Json.obj("type" -> "string")
      )
    }

    "resolve dependencies constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"dependencies": {
            |    "a": "b",
            |    "c": ["d", "e"]
            |  }
            |}""".stripMargin).get

        val context = Context(schema)
        val resolved = RefResolver.resolve("#/dependencies/c/1", context)
        Json.toJson(resolved.get) must beEqualTo(JsString("e"))
      }

    "resolve patternProperties constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"patternProperties": {
            |        "^(/[^/]+)+$": {}
            |}
            |}""".stripMargin).get

        val context = Context(schema)

        val resolved = RefResolver.resolve("#/patternProperties", context)
        Json.toJson(
          resolved.get) must beAnInstanceOf[JsObject]
      }
    "should resolve additionalProperties constraint" in new
        WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

      val schema = JsonSource.schemaFromString(
        """{
          |"additionalProperties": false
          |}""".stripMargin).get

      val context = Context(schema)
      val resolved = RefResolver.resolve("#/additionalProperties", context)
      Json.toJson(resolved.get) must beEqualTo(JsBoolean(false))
    }

    "should resolve definitions constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        GlobalContextCache.clear()
        val schema = JsonSource.schemaFromString(
          """{
            |"type": "boolean",
            |"definitions": {
            |  "foo": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
            |}
            |}""".stripMargin).get

        val context = Context(schema)
        val resolved = RefResolver.resolve("#/definitions/foo", context)
        Json.toJson(resolved.get) must beEqualTo(Json.obj("type" -> "string", "minLength" -> 10, "maxLength" -> 20))
      }

    "should resolve additionalProperties constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        GlobalContextCache.clear()
        val schema = JsonSource.schemaFromString(
          """{
            |"id": "http://localhost:1234/talk.json#",
            |"definitions": {
            |  "foo": {
            |    "id": "schema1",
            |    "type": "integer"
            |  }
            |},
            |"properties": {
            |  "foo": {
            |    "$ref": "date.json#/properties/year"
            |  }
            |}
            |}""".
            stripMargin).get

        val result = SchemaValidator.validate(schema, Json.obj("foo" -> JsNumber(2015)))
        result.isSuccess must beTrue
        val result2 = SchemaValidator.validate(schema, Json.obj("foo" -> JsString("foo")))
        result2.isFailure must beTrue

      }
  }
}
