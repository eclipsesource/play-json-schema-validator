package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.SchemaRefResolver
import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema.test.Assets
import com.osinka.i18n.Lang
import org.specs2.mutable.Specification
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.test.WithServer

import scalaz.{\/, \/-}
import scalaz.syntax.either._

class SchemaRefResolverSpec extends Specification {

  def createApp: Application = new GuiceApplicationBuilder()
    .routes(Assets.routes(getClass)).build()

  val resolver = new SchemaRefResolver

  def resolveSchema(ref: String, scope: SchemaResolutionScope)
                   (implicit lang: Lang = Lang.Default): Either[JsonValidationError, ResolvedResult[SchemaType]] = {

    val refTypeClass = SchemaRefResolver.schemaRefInstance
    def hasRef(obj: SchemaType)  = refTypeClass.findRef(obj).isDefined

    if (hasRef(scope.documentRoot)) {
      val x: Option[\/[JsonValidationError, ResolvedResult[SchemaType]]] = refTypeClass.findRef(scope.documentRoot).map(r =>
        resolver.resolve(scope.documentRoot, r, scope.addVisited(r)) match {
          case \/-(ResolvedResult(resolved, s)) => resolver.resolve(resolved, Ref(ref), s)
          case (err) => err
        }
      )
      x.getOrElse(resolver.resolutionFailure(Ref(ref))(scope).left).toEither
    } else {
      resolver.resolve(scope.documentRoot, Ref(ref), scope).toEither
    }
  }

  private def resolvedToJson(resolvedResult: ResolvedResult[SchemaType]) =
    Json.toJson(resolvedResult.resolved)

  "SchemaRefResolver" should {

    "resolve references into JSON schema v4 draft" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "$ref": "http://json-schema.org/draft-04/schema#"
          |}""".stripMargin).get

      val context = new SchemaResolutionScope(schema)
      val resolver = new SchemaRefResolver
      val arrayDef = resolveSchema("#/definitions/schemaArray", context)
      val anyOf = resolveSchema("#/properties/anyOf", context)
      anyOf.map(_.resolved) must beRight.which(_.isInstanceOf[SchemaArray])
      arrayDef.map(_.resolved) must beRight.which(_.isInstanceOf[SchemaArray])
    }

    "be able to resolve JSON Pointer examples" in {

      val schema = SchemaObject(
        Seq(
          SchemaAttribute("foo",
            SchemaTuple(
              Seq(
                SchemaValue(JsString("bar")), SchemaValue(JsString("baz"))
              )
            )
          ),
          SchemaAttribute("", SchemaValue(JsNumber(0))),
          SchemaAttribute("a/b", SchemaValue(JsNumber(1))),
          SchemaAttribute("c%d", SchemaValue(JsNumber(2))),
          SchemaAttribute("e^f", SchemaValue(JsNumber(3))),
          SchemaAttribute("g|h", SchemaValue(JsNumber(4))),
          SchemaAttribute("i\\j", SchemaValue(JsNumber(5))),
          SchemaAttribute("k\'l", SchemaValue(JsNumber(6))),
          SchemaAttribute(" ", SchemaValue(JsNumber(7))),
          SchemaAttribute("m~n", SchemaValue(JsNumber(8)))
        )
      )

      val scope = new SchemaResolutionScope(schema)
      resolveSchema("#/foo/0", scope).map(_.resolved) must beRight(SchemaValue(JsString("bar")))
      resolveSchema("#/", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(0)))
      resolveSchema("#/a~1b", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(1)))
      // TODO: fails
      // resolver.resolveValue("#/c%d", scope)   must beRight(SchemaValue(JsNumber(2)))
      resolveSchema("#/e^f", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(3)))
      resolveSchema("#/g|h", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(4)))
      resolveSchema("#/i\\j", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(5)))
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
      resolveSchema("#/minItems", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(42)))
      resolveSchema("#/maxItems", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(99)))
      resolveSchema("#/additionalItems", scope).map(_.resolved) must beRight(SchemaValue(JsBoolean(false)))
      resolveSchema("#/uniqueItems", scope).map(_.resolved) must beRight(SchemaValue(JsBoolean(false)))
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
      resolveSchema("#/minimum", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(0)))
      resolveSchema("#/maximum", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(10)))
      resolveSchema("#/multipleOf", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(2)))
    }

    "resolve string constraints" in {
      val schema =
        JsonSource.schemaFromString(
          """{
            |  "type": "string",
            |  "minLength": 1,
            |  "maxLength": 10,
            |  "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
            |}""".stripMargin).get

      val scope = new SchemaResolutionScope(schema)
      resolveSchema("#/minLength", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(1)))
      resolveSchema("#/maxLength", scope).map(_.resolved) must beRight(SchemaValue(JsNumber(10)))
      resolveSchema("#/pattern", scope).map(_.resolved) must beRight(SchemaValue(JsString("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")))
    }

    "resolve anyOf constraint" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val resolved = resolveSchema("#/anyOf/0", new SchemaResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(
          Json.obj(
            "type" -> "string",
            "minLength" -> 10,
            "maxLength" -> 20
          )
        )
      }

    "resolve oneOf constraint" in
      new WithServer(app = createApp, port = 1234) {
        val schema = JsonSource.schemaFromString(
          """{
            |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".stripMargin).get

        val resolved = resolveSchema("#/oneOf/0", new SchemaResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(
          Json.obj(
            "type" -> "string",
            "minLength" -> 10,
            "maxLength" -> 20
          )
        )
      }

    "resolve allOf constraint" in
      new WithServer(app = createApp, port = 1234) {
        val schema = JsonSource.schemaFromString(
          """{
            |"allOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
            |}""".
            stripMargin).get

        val resolved = resolveSchema("#/allOf/0", new SchemaResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(
          Json.obj(
            "type" ->
              "string",
            "minLength" -> 10,
            "maxLength" -> 20
          )
        )
      }

    "resolve definitions constraint" in
      new WithServer(app = createApp, port = 1234) {
        val schema = JsonSource.schemaFromString(
          """{
            |  "definitions": {
            |    "foo": { "type": "string" }
            |  }
            |}""".
            stripMargin).get
        val resolved = resolveSchema("#/definitions/foo", new SchemaResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(
          Json.obj("type" -> "string")
        )
      }

    "should resolve definitions constraint" in
      new WithServer(app = createApp, port = 1234) {
        val schema = JsonSource.schemaFromString(
          """{
            |  "minLength": 10,
            |  "oneOf": [{
            |    "foo": { "type": "string" }
            |  }]
            |}""".stripMargin).get
        val resolved = resolveSchema("#/oneOf/0/foo", new SchemaResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(
          Json.obj("type" -> "string")
        )
      }

    "resolve dependencies constraint" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"dependencies": {
            |    "a": "b",
            |    "c": ["d", "e"]
            |  }
            |}""".stripMargin).get

        val resolved = resolveSchema("#/dependencies/c/1", new SchemaResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(JsString("e"))
      }

    "resolve patternProperties constraint" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"patternProperties": {
            |        "^(/[^/]+)+$": {}
            |}
            |}""".stripMargin).get

        val result = resolveSchema("#/patternProperties", new SchemaResolutionScope(schema))
        result.map(resolvedToJson).right.get must beAnInstanceOf[JsObject]
      }

    "should resolve additionalProperties constraint" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"additionalProperties": false
            |}""".stripMargin).get

        val result = resolveSchema("#/additionalProperties", new SchemaResolutionScope(schema))
        result.map(resolvedToJson) must beRight(JsBoolean(false))
      }

    "should resolve definitions constraint" in
      new WithServer(app = createApp, port = 1234) {

        val schema = JsonSource.schemaFromString(
          """{
            |"type": "boolean",
            |"definitions": {
            |  "foo": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
            |}
            |}""".stripMargin).get

        val resolved = resolveSchema("#/definitions/foo", GenResolutionScope(schema))
        resolved.right.map(resolvedToJson) must beRight(
          Json.obj(
            "type" -> "string",
            "minLength" -> 10,
            "maxLength" -> 20
          )
        )
      }

    "resolve additionalProperties constraint" in
      new WithServer(app = createApp, port = 1234) {

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
          Json.obj(
            "foo" -> JsNumber(2015)
          )
        ).isSuccess must beTrue

        SchemaValidator().validate(schema,
          Json.obj("foo"
            -> JsString("foo"))
        ).isError must beTrue
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
      val failedResult = resolveSchema("#/properties/schema1/notthere", context)
      failedResult must beLeft.which(err => err.message === "Could not resolve ref #/properties/schema1/notthere.")
      val result = resolveSchema("#/properties/schema1/type", context)
      result must beRight.which(r => r.scope.id === Some(Ref("http://x.y.z/rootschema.json#foo")))
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
          |      "$ref": "#/definitions/foo"
          |    }
          |  }
          |}""".
          stripMargin).get

      val context = new SchemaResolutionScope(schema)
      val result = resolveSchema("#/properties/n", context)
      result must beRight.which(r => r.resolved.isInstanceOf[SchemaNumber])
    }

    "find resolution scope of schema1" in {
      val schema = JsonSource.schemaFromString(
        """{
          |     "id": "http://my.site/myschema#",
          |     "definitions": {
          |         "schema1": {
          |             "id": "schema1",
          |             "type": "integer"
          |         },
          |         "schema2": {
          |             "type": "array",
          |             "items": { "$ref": "schema1" }
          |         }
          |     }
          |}""".stripMargin).get

      val scope = new SchemaResolutionScope(schema)
      val result = resolveSchema("http://my.site/schema1#", scope)
      result must beRight.which(r => r.resolved.isInstanceOf[SchemaInteger])
    }

    "resolve nil fragments" in {
      val schema = JsonSource.schemaFromString("{}").get
      // scope and schema do not matter in this test
      implicit val lang = Lang.Default
      val result = resolver.resolveLocal(Nil, new SchemaResolutionScope(schema), schema)
      result.toEither must beRight.which(r => r.resolved == schema )
    }

    "resolve refs within mySiteSchema" in {

      val mySiteSchema = JsonSource.schemaFromString(
        """{
          |     "id": "http://my.site/myschema#",
          |     "definitions": {
          |         "schema1": {
          |             "properties": {
          |                 "foo": {
          |                   "id": "schema1",
          |                   "type": "integer"
          |                 }
          |             }
          |         },
          |         "schema2": {
          |             "type": "array",
          |             "items": { "$ref": "schema1" }
          |         }
          |     }
          |}""".
          stripMargin
      ).get
      val scope = new SchemaResolutionScope(mySiteSchema)

      "resolve schema1 via anchor" in {
        resolveSchema("http://my.site/schema1#", scope) must beRight.which(r =>
          r.resolved.isInstanceOf[SchemaInteger]
        )
      }

      "resolve schema1 via full path" in {
        val result = resolveSchema("http://my.site/myschema#/definitions/schema1", scope)
        result.right.map(resolvedToJson) must beRight(
          Json.obj(
            "properties" -> Json.obj(
              "foo" -> Json.obj(
                "id" -> "schema1",
                "type" -> "integer"
              )
            )
          )
        )
      }

      "resolve type of schema2 via full path" in {
        val result = resolveSchema("http://my.site/myschema#/definitions/schema2/type", scope)
        result.right.map(resolvedToJson) must beRight(JsString("array"))
      }
    }

    "resolve examples from http://json-schema.org/latest/json-schema-core.html#anchor27" in {
      val schema = JsonSource.schemaFromString(
        """{
          |    "id": "http://x.y.z/rootschema.json#",
          |    "schema1": {
          |        "id": "#foo"
          |    },
          |    "schema2": {
          |        "id": "otherschema.json",
          |        "nested": {
          |            "id": "#bar"
          |        },
          |        "alsonested": {
          |            "id": "t/inner.json#a"
          |        }
          |    },
          |    "schema3": {
          |        "id": "some://where.else/completely#"
          |    }
          |}
        """.stripMargin).get
      // resolution scope checks --
      val scope = new SchemaResolutionScope(schema, id = Some(Ref("http://x.y.z/rootschema.json#")))

      "infer correct resolution scope for #" in {
        resolveSchema("#", scope) must beRight.which(
          _.scope.id.contains(Ref("http://x.y.z/rootschema.json#"))
        )
      }

      "infer correct resolution scope within schema1" in {
        resolveSchema("#/schema1/id", scope) must beRight.which(
          _.scope.id.contains(Ref("http://x.y.z/rootschema.json#foo"))
        )
      }

      "infer correct resolution scope within schema2" in {
        resolveSchema("#/schema2", scope) must beRight.which(
          _.scope.id.contains(Ref("http://x.y.z/otherschema.json#"))
        )
      }

      "infer correct resolution scope within schema2/nested" in {
        resolveSchema("#/schema2/nested/id", scope) must beRight.which(
          _.scope.id.contains(Ref("http://x.y.z/otherschema.json#bar"))
        )
      }

      "infer correct resolution scope within schema2/alsonested" in {
        val r = resolveSchema("#/schema2/alsonested/id", scope)
        r must beRight.which(
          _.scope.id.contains(Ref("http://x.y.z/t/inner.json#a"))
        )
      }

      "infer correct resolution scope within schema3" in {
        resolveSchema("#/schema3/id", scope) must beRight.which(
          _.scope.id.contains(Ref("some://where.else/completely#"))
        )

        "resolve #/schema1" in {
          val result = resolveSchema("#/schema1", scope)
          result.right.map { case ResolvedResult(r, s) => Json.toJson(r) } must beRight(Json.obj("id" -> "#foo"))
        }

        "resolve #/schema2" in {
          val result = resolveSchema("#/schema2", scope)
          result.right.map(resolvedToJson) must beRight(
            Json.obj(
              "nested" -> Json.obj("id" -> "#bar"),
              "alsonested" -> Json.obj("id" -> "t/inner.json#a"),
              "id" -> "otherschema.json"
            )
          )
        }

        "resolve #/schema2/nested" in {
          val result = resolveSchema("#/schema2/nested", scope)
          result.right.map(resolvedToJson) must beRight(
            Json.obj("id" -> "#bar")
          )
        }

        "resolve #/schema2/alsonested" in {
          val result = resolveSchema("#/schema2/alsonested", scope)
          result.right.map(resolvedToJson) must beRight(
            Json.obj("id" -> "t/inner.json#a")
          )
        }

        "resolve #/schema3" in {
          val result = resolveSchema("#/schema3", scope)
          result.right.map(resolvedToJson) must beRight(
            Json.obj("id" -> "some://where.else/completely#")
          )
        }
      }
    }
  }
}
