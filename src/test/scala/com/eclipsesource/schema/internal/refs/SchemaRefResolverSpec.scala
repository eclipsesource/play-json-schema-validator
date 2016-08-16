package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema._
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.data.validation.ValidationError
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
      resolver.resolveSchema("#/foo/0", scope) must beRight(SchemaValue(JsString("bar")))
      resolver.resolveSchema("#/", scope)      must beRight(SchemaValue(JsNumber(0)))
      resolver.resolveSchema("#/a~1b", scope)  must beRight(SchemaValue(JsNumber(1)))
      // TODO: fails
      //      resolver.resolveValue("#/c%d", scope)   must beRight(SchemaValue(JsNumber(2)))
      resolver.resolveSchema("#/e^f", scope)   must beRight(SchemaValue(JsNumber(3)))
      resolver.resolveSchema("#/g|h", scope)   must beRight(SchemaValue(JsNumber(4)))
      resolver.resolveSchema("#/i\\j", scope)  must beRight(SchemaValue(JsNumber(5)))
    }
  }

  "normalize path with remote document root" in {
    val someSchema = SchemaNull()
    val root  = "http://x.y.z/rootschema.json#"
    val other = "http://x.y.z/otherschema.json#"

    resolver.normalize(Pointer("#foo"),
      new SchemaResolutionScope(someSchema,  Some(Pointer(root)))) must beEqualTo(Pointer("http://x.y.z/rootschema.json#foo"))

    // TODO meh
    resolver.normalize(Pointer("otherschema.json"),
      new SchemaResolutionScope(someSchema, Some(Pointer(other)))) must beEqualTo(Pointer("http://x.y.z/otherschema.json#"))

    resolver.normalize(Pointer("#bar"),
      new SchemaResolutionScope(someSchema, Some(Pointer(other)))) must beEqualTo(Pointer("http://x.y.z/otherschema.json#bar"))

    resolver.normalize(Pointer("t/inner.json#a"),
      new SchemaResolutionScope(someSchema, Some(Pointer(other)))) must beEqualTo(Pointer("http://x.y.z/t/inner.json#a"))
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
    resolver.resolveSchema("#/minItems", scope) must beRight(SchemaValue(JsNumber(42)))
    resolver.resolveSchema("#/maxItems", scope) must beRight(SchemaValue(JsNumber(99)))
    resolver.resolveSchema("#/additionalItems", scope) must beRight(SchemaValue(JsBoolean(false)))
    resolver.resolveSchema("#/uniqueItems", scope) must beRight(SchemaValue(JsBoolean(false)))
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
    resolver.resolveSchema("#/minimum", scope) must beRight(SchemaValue(JsNumber(0)))
    resolver.resolveSchema("#/maximum", scope) must beRight(SchemaValue(JsNumber(10)))
    resolver.resolveSchema("#/multipleOf", scope) must beRight(SchemaValue(JsNumber(2)))
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
    resolver.resolveSchema("#/minLength", scope) must beRight(SchemaValue(JsNumber(1)))
    resolver.resolveSchema("#/maxLength", scope) must beRight(SchemaValue(JsNumber(10)))
    resolver.resolveSchema("#/pattern", scope) must beRight(SchemaValue(JsString("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")))
  }

  "resolve anyOf constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {

      val schema = JsonSource.schemaFromString(
        """{
          |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val resolved = resolver.resolveSchema("#/anyOf/0", new SchemaResolutionScope(schema))
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

      val resolved = resolver.resolveSchema("#/oneOf/0", new SchemaResolutionScope(schema))
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
      val resolved = resolver.resolveSchema("#/allOf/0", new SchemaResolutionScope(schema))
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
    val resolved = resolver.resolveSchema("#/definitions/foo", new SchemaResolutionScope(schema))
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
      val resolved = resolver.resolveSchema("#/oneOf/0/foo", new SchemaResolutionScope(schema))
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
      val resolved = resolver.resolveSchema("#/dependencies/c/1", new SchemaResolutionScope(schema))
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
      val resolved = resolver.resolveSchema("#/patternProperties", new SchemaResolutionScope(schema))
      Json.toJson(resolved.right.get) must beAnInstanceOf[JsObject]
    }

  "should resolve additionalProperties constraint" in
    new WithServer(app = new GuiceApplicationBuilder().routes(routes).build(), port = 1234) {
      val schema = JsonSource.schemaFromString(
        """{
          |"additionalProperties": false
          |}""".stripMargin).get

      val resolved = resolver.resolveSchema("#/additionalProperties", new SchemaResolutionScope(schema))
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

      val resolved = resolver.resolveSchema("#/definitions/foo", GenResolutionScope(schema))
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
    val arrayDef: Either[ValidationError, SchemaType] = resolver.resolveSchema("#/definitions/schemaArray", context)
    val anyOf = resolver.resolveSchema("#/properties/anyOf", context)
    anyOf must beRight.which(_.isInstanceOf[SchemaArray])
    arrayDef must beRight.which(_.isInstanceOf[SchemaArray])
  }

  "resolve type keyword" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "id": "http://x.y.z/rootschema.json#",
        |  "type": "object",
        |  "properties": {
        |    "schema1": {
        |      "type": "object",
        |      "id": "#foo"
        |    }
        |  }
        |}""".
        stripMargin).get

    val context = new SchemaResolutionScope(schema)
    val failedResult = resolver.resolve(Pointer("#/properties/schema1/notthere"), context)
    failedResult must beLeft.which(err => err.message === "Could not resolve ref #/properties/schema1/notthere")
    val result = resolver.resolve(Pointer("#/properties/schema1/type"), context)
    result must beRight.which(r => r.scope.id === Some(Pointer("http://x.y.z/rootschema.json#foo")))
  }

  "resolve ref via fragment" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "object",
        |  "definitions": {
        |    "foo": {
        |      "$ref": "#/definitions/bar"
        |    },
        |    "bar": {
        |      "minimum": 10
        |    }
        |  },
        |  "properties": {
        |    "n": {
        |      "$ref": "#/definitions/foo/bar"
        |    }
        |  }
        |}""".
        stripMargin).get
    val context = new

        SchemaResolutionScope(schema)
    val result =
      resolver.resolve(Pointer("#/properties/n"), context)
    result must
      beRight.which(r => r.resolved.isInstanceOf[SchemaNumber])
  }

}

