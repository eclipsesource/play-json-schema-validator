package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema._
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.WithServer

class SchemaRefResolverSpec extends Specification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/", path)
  }

  val resolver = new SchemaRefResolver

  "SchemaRefResolver" should {

    "be able to resolve all draft examples" in {

      val schema = SchemaObject(
        Seq(
          SchemaAttribute("foo",
            SchemaTuple(
              Seq(
                SchemaValue(JsString("bar")), SchemaValue(JsString("baz"))
              )
            )
          ),
          SchemaAttribute("",     SchemaValue(JsNumber(0))),
          SchemaAttribute("a/b",  SchemaValue(JsNumber(1))),
          SchemaAttribute("c%d",  SchemaValue(JsNumber(2))),
          SchemaAttribute("e^f",  SchemaValue(JsNumber(3))),
          SchemaAttribute("g|h",  SchemaValue(JsNumber(4))),
          SchemaAttribute("i\\j", SchemaValue(JsNumber(5))),
          SchemaAttribute("k\'l", SchemaValue(JsNumber(6))),
          SchemaAttribute(" ",    SchemaValue(JsNumber(7))),
          SchemaAttribute("m~n",  SchemaValue(JsNumber(8)))
        )
      )

      val scope = new SchemaResolutionScope(schema)
      resolver.resolve("#/foo/0", scope) must beRight(SchemaValue(JsString("bar")))
      resolver.resolve("#/", scope)      must beRight(SchemaValue(JsNumber(0)))
      resolver.resolve("#/a~1b", scope)  must beRight(SchemaValue(JsNumber(1)))
      // TODO: fails
      //      resolver.resolve("#/c%d", scope)   must beRight(SchemaValue(JsNumber(2)))
      resolver.resolve("#/e^f", scope)   must beRight(SchemaValue(JsNumber(3)))
      resolver.resolve("#/g|h", scope)   must beRight(SchemaValue(JsNumber(4)))
      resolver.resolve("#/i\\j", scope)  must beRight(SchemaValue(JsNumber(5)))
    }
  }

  "normalize path with remote document root" in {
    val someSchema = SchemaNull()
    val root = "http://x.y.z/rootschema.json#"
    val other = "http://x.y.z/otherschema.json#"

    resolver.normalize("#foo",
      new SchemaResolutionScope(someSchema,  Some(root), Some(root))) must beEqualTo("http://x.y.z/rootschema.json#foo")

    resolver.normalize("otherschema.json",
      new SchemaResolutionScope(someSchema, Some(other), Some(other))) must beEqualTo("http://x.y.z/otherschema.json#")

    resolver.normalize("#bar",
      new SchemaResolutionScope(someSchema, Some(other), Some(other))) must beEqualTo("http://x.y.z/otherschema.json#bar")

    resolver.normalize("t/inner.json#a",
      new SchemaResolutionScope(someSchema, Some(other), Some(other))) must beEqualTo("http://x.y.z/t/inner.json#a")
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
    val scope = new SchemaResolutionScope(schema)
    resolver.resolve("#/minItems", scope) must beRight(SchemaValue(JsNumber(42)))
    resolver.resolve("#/maxItems", scope) must beRight(SchemaValue(JsNumber(99)))
    resolver.resolve("#/additionalItems", scope) must beRight(SchemaValue(JsBoolean(false)))
    resolver.resolve("#/uniqueItems", scope) must beRight(SchemaValue(JsBoolean(false)))
  }

  "resolve number constraints" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "integer",
        |  "minimum": 0,
        |  "maximum": 10,
        |  "multipleOf": 2
        |}""".stripMargin).get
    val scope = new SchemaResolutionScope(schema)
    resolver.resolve("#/minimum", scope) must beRight(SchemaValue(JsNumber(0)))
    resolver.resolve("#/maximum", scope) must beRight(SchemaValue(JsNumber(10)))
    resolver.resolve("#/multipleOf", scope) must beRight(SchemaValue(JsNumber(2)))
  }

  "resolve string constraints" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "string",
        |  "minLength": 1,
        |  "maxLength": 10,
        |  "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
        |}""".stripMargin).get
    val scope = new SchemaResolutionScope(schema)
    resolver.resolve("#/minLength", scope) must beRight(SchemaValue(JsNumber(1)))
    resolver.resolve("#/maxLength", scope) must beRight(SchemaValue(JsNumber(10)))
    resolver.resolve("#/pattern", scope) must beRight(SchemaValue(JsString("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")))
  }

  "resolve anyOf constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {

      val schema = JsonSource.schemaFromString(
        """{
          |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val resolved = resolver.resolve("#/anyOf/0", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(
        Json.obj(
          "type" -> "string",
          "minLength" -> 10,
          "maxLength" -> 20
        )
      )
    }

  "resolve oneOf constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val resolved = resolver.resolve("#/oneOf/0", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(
        Json.obj(
          "type" -> "string",
          "minLength" -> 10,
          "maxLength" -> 20
        )
      )
    }

  "resolve allOf constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"allOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".
          stripMargin).get
      val resolved = resolver.resolve("#/allOf/0", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(
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
        new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
    val schema = JsonSource.schemaFromString(
      """{
        |  "definitions": {
        |    "foo": { "type": "string" }
        |  }
        |}""".stripMargin).get
    val resolved = resolver.resolve("#/definitions/foo", new SchemaResolutionScope(schema))
    Json.toJson(resolved.right.get) must beEqualTo(
      Json.obj("type" -> "string")
    )
  }

  "should resolve definitions constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |  "minLength": 10,
          |  "oneOf": [{
          |    "foo": { "type": "string" }
          |  }]
          |}""".stripMargin).get
      val resolved = resolver.resolve("#/oneOf/0/foo", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(
        Json.obj("type" -> "string")
      )
    }

  "resolve dependencies constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"dependencies": {
          |    "a": "b",
          |    "c": ["d", "e"]
          |  }
          |}""".stripMargin).get
      val resolved = resolver.resolve("#/dependencies/c/1", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(JsString("e"))
    }

  "resolve patternProperties constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"patternProperties": {
          |        "^(/[^/]+)+$": {}
          |}
          |}""".stripMargin).get
      val resolved = resolver.resolve("#/patternProperties", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beAnInstanceOf[JsObject]
    }

  "should resolve additionalProperties constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"additionalProperties": false
          |}""".stripMargin).get

      val resolved = resolver.resolve("#/additionalProperties", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(JsBoolean(false))
    }

  "should resolve definitions constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"type": "boolean",
          |"definitions": {
          |  "foo": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
          |}
          |}""".stripMargin).get

      val resolved = resolver.resolve("#/definitions/foo", GenResolutionScope(schema))
      Json.toJson(resolved.right.get) must beEqualTo(Json.obj("type" -> "string", "minLength" -> 10, "maxLength" -> 20))
    }

  "resolve additionalProperties constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
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

      SchemaValidator().validate(schema,
        Json.obj("foo" -> JsNumber(2015))
      ).isSuccess must beTrue

      SchemaValidator().validate(schema,
        Json.obj("foo" -> JsString("foo"))
      ).isError must beTrue
    }

  "resolve references into JSON schema v4 draft" in {

    val schema = JsonSource.schemaFromString(
      """{
        |  "$ref": "http://json-schema.org/draft-04/schema#"
        |}""".stripMargin).get

    val context = new SchemaResolutionScope(schema)
    val resolver = new SchemaRefResolver
    val arrayDef = resolver.resolve("#/definitions/schemaArray", context)
    val anyOf = resolver.resolve("#/properties/anyOf", context)
    arrayDef must beRight.which(_.isInstanceOf[SchemaArray])
    anyOf must beRight.which(_.isInstanceOf[SchemaArray])
  }
}
