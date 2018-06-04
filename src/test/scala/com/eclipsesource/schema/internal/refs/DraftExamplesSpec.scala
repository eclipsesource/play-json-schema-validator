package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.drafts.Version4
import com.eclipsesource.schema.internal._
import com.eclipsesource.schema.{JsonSource, SchemaInteger}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsString, JsValue, Json}

class DraftExamplesSpec extends Specification {

  import Version4._

  "Example from draft 4 - section 7.2.2" should {

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

    val resolver = SchemaRefResolver(Version4)
    val scope = SchemaResolutionScope(schema).copy(id = Some(Ref("http://x.y.z/rootschema.json#")))

    "infer correct resolution scope for #" in {
      resolver.resolveFromRoot("#", scope) must beRight.which(
        _.scope.id.contains(Ref("http://x.y.z/rootschema.json#"))
      )
    }

    "infer correct resolution scope within schema1" in {
      resolver.resolveFromRoot("#/schema1", scope) must beRight.which(
        _.scope.id.contains(Ref("http://x.y.z/rootschema.json#foo"))
      )
    }

    "infer correct resolution scope within schema2" in {
      resolver.resolveFromRoot("#/schema2", scope) must beRight.which(
        _.scope.id.contains(Ref("http://x.y.z/otherschema.json#"))
      )
    }

    "infer correct resolution scope within schema2/nested" in {
      resolver.resolveFromRoot("#/schema2/nested", scope) must beRight.which(
        _.scope.id.contains(Ref("http://x.y.z/otherschema.json#bar"))
      )
    }

    "infer correct resolution scope within schema2/alsonested" in {
      resolver.resolveFromRoot("#/schema2/alsonested", scope) must beRight.which(
        _.scope.id.contains(Ref("http://x.y.z/t/inner.json#a"))
      )
    }

    "infer correct resolution scope within schema3" in {
      resolver.resolveFromRoot("#/schema3", scope) must beRight.which(
        _.scope.id.contains(Ref("some://where.else/completely#"))
      )
    }

    "resolve #/schema1" in {
      val result = resolver.resolveFromRoot("#/schema1", scope)
      result.right.map(_.toJson) must beRight[JsValue](Json.obj("id" -> "#foo"))
    }

    "resolve #/schema2" in {
      val result = resolver.resolveFromRoot("#/schema2", scope)
      result.right.map(_.toJson) must beRight[JsValue](
        Json.obj(
          "nested" -> Json.obj("id" -> "#bar"),
          "alsonested" -> Json.obj("id" -> "t/inner.json#a"),
          "id" -> "otherschema.json"
        )
      )
    }

    "resolve #/schema2/nested" in {
      val result = resolver.resolveFromRoot("#/schema2/nested", scope)
      result.right.map(_.toJson) must beRight[JsValue](
        Json.obj("id" -> "#bar")
      )
    }

    "resolve #/schema2/alsonested" in {
      val result = resolver.resolveFromRoot("#/schema2/alsonested", scope)
      result.right.map(_.toJson) must beRight[JsValue](
        Json.obj("id" -> "t/inner.json#a")
      )
    }

    "resolve #/schema3" in {
      val result = resolver.resolveFromRoot("#/schema3", scope)
      result.right.map(_.toJson) must beRight[JsValue](
        Json.obj("id" -> "some://where.else/completely#")
      )
    }
  }

  "Example from draft 4 - section 7.2.3" should {

    val mySiteSchema = JsonSource.schemaFromString(
      """{
        |  "id": "http://my.site/myschema#",
        |  "definitions": {
        |    "schema1": {
        |      "properties": {
        |        "foo": {
        |          "id": "schema1",
        |          "type": "integer"
        |        }
        |      }
        |    },
        |    "schema2": {
        |      "type": "array",
        |      "items": { "$ref": "schema1" }
        |    }
        |  }
        |}""".
        stripMargin
    ).get
    val resolver = SchemaRefResolver(
      Version4,
      DocumentCache().addAll(collectSchemas(mySiteSchema, Some(Ref("http://my.site/myschema#"))))
    )
    val scope = SchemaResolutionScope(mySiteSchema)

    "resolve schema1 via plain name fragment" in {
      resolver.resolveFromRoot("http://my.site/schema1#", scope) must beRight.which(
        _.resolved.isInstanceOf[SchemaInteger]
      )
    }

    "resolve schema1 via absolute path" in {
      val result = resolver.resolveFromRoot("http://my.site/myschema#/definitions/schema1", scope)
      result.right.map(_.toJson) must beRight[JsValue](
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

    "resolve schema2/type via absolute path" in {
      val result = resolver.resolveFromRoot("http://my.site/myschema#/definitions/schema2/type", scope)
      result.right.map(_.toJson) must beRight[JsValue](JsString("array"))
    }
  }
}

