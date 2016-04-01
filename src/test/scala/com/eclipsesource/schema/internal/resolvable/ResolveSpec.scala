package com.eclipsesource.schema.internal.resolvable

import com.eclipsesource.schema.internal.{GlobalContextCache, RefResolver, Context}
import com.eclipsesource.schema.{SchemaNull, SchemaValue, SchemaValidator, JsonSource}
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.{FakeApplication, WithServer}

class ResolveSpec extends Specification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }


  "RefResolver" should {

    "normalize path with remote document root" in {
      val someSchema = SchemaNull()
      val root = "http://x.y.z/rootschema.json#"
      val other = "http://x.y.z/otherschema.json#"

      RefResolver.normalize("#foo",
        Context(someSchema, Some(root), Some(root))) must beEqualTo("http://x.y.z/rootschema.json#foo")

      RefResolver.normalize("otherschema.json",
        Context(someSchema, Some(other), Some(other))) must beEqualTo("http://x.y.z/otherschema.json#")

      RefResolver.normalize("#bar",
        Context(someSchema, Some(other), Some(other))) must beEqualTo("http://x.y.z/otherschema.json#bar")

      RefResolver.normalize("t/inner.json#a",
        Context(someSchema, Some(other), Some(other))) must beEqualTo("http://x.y.z/t/inner.json#a")
    }

    "resolve array constraints" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "items": {
          |    "type": "integer"
          |  },
          |  "minItems": 42,
          |  "maxItems": 99,
          |  "additionalItems": false,
          |  "uniqueItems": false
          |}""".stripMargin).get
      RefResolver.resolve("#/minItems", Context(schema)) must beSome(SchemaValue(JsNumber(42)))
      RefResolver.resolve("#/maxItems", Context(schema)) must beSome(SchemaValue(JsNumber(99)))
      RefResolver.resolve("#/additionalItems", Context(schema)) must beSome(SchemaValue(JsBoolean(false)))
      RefResolver.resolve("#/uniqueItems", Context(schema)) must beSome(SchemaValue(JsBoolean(false)))
    }

    "resolve number constraints" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "integer",
          |  "minimum": 0,
          |  "maximum": 10,
          |  "multipleOf": 2
          |}""".stripMargin).get
      RefResolver.resolve("#/minimum", Context(schema)) must beSome(SchemaValue(JsNumber(0)))
      RefResolver.resolve("#/maximum", Context(schema)) must beSome(SchemaValue(JsNumber(10)))
      RefResolver.resolve("#/multipleOf", Context(schema)) must beSome(SchemaValue(JsNumber(2)))
    }

    "resolve string constraints" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "string",
          |  "minLength": 1,
          |  "maxLength": 10,
          |  "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
          |}""".stripMargin).get
      RefResolver.resolve("#/minLength", Context(schema)) must beSome(SchemaValue(JsNumber(1)))
      RefResolver.resolve("#/maxLength", Context(schema)) must beSome(SchemaValue(JsNumber(10)))
      RefResolver.resolve("#/pattern", Context(schema)) must beSome(SchemaValue(JsString("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")))
    }

    "resolve anyOf constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val context = Context(schema)

        val resolved = RefResolver.resolve("#/anyOf/0", context)

        Json.toJson(RefResolver.resolveRefIfAny(context)(resolved.get)) must beEqualTo(
          Json.obj(
            "type" -> "string",
            "minLength" -> 10,
            "maxLength" -> 20
          )
        )
      }

    "resolve oneOf constraint" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val context = Context(schema)
      val resolved = RefResolver.resolve("#/oneOf/0", context)

      Json.toJson(RefResolver.resolveRefIfAny(context)(resolved.get)) must beEqualTo(
        Json.obj(
          "type" -> "string",
          "minLength" -> 10,
          "maxLength" -> 20
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

      val resolved = RefResolver.resolve("#/allOf/0", context)
      Json.toJson(RefResolver.resolveRefIfAny(context)(resolved.get)) must beEqualTo(
        Json.obj(
          "type" ->
            "string",
          "minLength" -> 10,
          "maxLength" -> 20
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

    "resolve additionalProperties constraint" in
      new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

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

        SchemaValidator.validate(schema,
          Json.obj("foo" -> JsNumber(2015))
        ).isSuccess must beTrue

        SchemaValidator.validate(schema,
          Json.obj("foo" -> JsString("foo"))
        ).isError must beTrue
      }
  }
}
