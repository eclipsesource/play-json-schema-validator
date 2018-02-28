package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.{JsonSource, SchemaArray, SchemaType}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsValue, Json}

class ResolveAnyConstraintsSpec extends Specification { self =>

  "SchemaRefResolver" should {

    import Version4._

    val talkSchema = JsonSource.schemaFromStream(
      self.getClass.getResourceAsStream("/talk.json")
    ).get

    val resolver = SchemaRefResolver(Version4)
      .copy(cache = DocumentCache()
        .add(Ref("http://localhost:1234/talk.json"))(talkSchema)
      )


    "resolve oneOf constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "oneOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val scope = SchemaResolutionScope(schema)
      val resolvedStringConstraint = resolver.resolveFromRoot("#/oneOf/0", scope)
      val resolvedMinLength = resolver.resolveFromRoot("#/oneOf/0/minLength", scope)
      resolvedStringConstraint.right.map(_.toJson) must beRight[JsValue](
        Json.obj(
          "type" -> "string",
          "minLength" -> 10,
          "maxLength" -> 20
        )
      )
      resolvedMinLength.right.map(_.toJson) must beRight[JsValue](JsNumber(10))
    }

    "resolve allOf constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"allOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".
          stripMargin).get

      val resolved = resolver.resolveFromRoot("#/allOf/0", SchemaResolutionScope(schema))
      resolved.right.map(_.toJson) must beRight[JsValue](
        Json.obj(
          "type" ->
            "string",
          "minLength" -> 10,
          "maxLength" -> 20
        )
      )
    }

    "resolve anyOf constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"anyOf": [{ "$ref": "http://localhost:1234/talk.json#/properties/title" }]
          |}""".stripMargin).get

      val resolved = resolver.resolveFromRoot("#/anyOf/0", SchemaResolutionScope(schema))
      resolved.right.map(_.toJson) must beRight[JsValue](
        Json.obj(
          "type" -> "string",
          "minLength" -> 10,
          "maxLength" -> 20
        )
      )
    }


    "resolve references into JSON schema v4 draft" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "$ref": "http://json-schema.org/draft-04/schema#"
          |}""".stripMargin).get

      val scope = SchemaResolutionScope(schema)
      val arrayDef = resolver.resolveFromRoot("#/definitions/schemaArray", scope)
      val anyOf = resolver.resolveFromRoot("#/properties/anyOf", scope)
      anyOf.map(_.resolved) must beRight[SchemaType].which(_.isInstanceOf[SchemaArray])
      arrayDef.map(_.resolved) must beRight[SchemaType].which(_.isInstanceOf[SchemaArray])
    }

    "resolve definitions constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "definitions": {
          |    "foo": { "type": "string" }
          |  }
          |}""".
          stripMargin).get
      val resolved = resolver.resolveFromRoot("#/definitions/foo", SchemaResolutionScope(schema))
      resolved.right.map(_.toJson) must beRight[JsValue](
        Json.obj("type" -> "string")
      )
    }

    "resolve definitions with $ref" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "boolean",
          |  "definitions": {
          |    "foo": { "$ref": "http://localhost:1234/talk.json#/properties/title" }
          |  }
          |}""".stripMargin).get

      val resolved = resolver.resolveFromRoot("#/definitions/foo", SchemaResolutionScope(schema))
      resolved.right.map(_.toJson) must beRight[JsValue](
        Json.obj(
          "type" -> "string",
          "minLength" -> 10,
          "maxLength" -> 20
        )
      )
    }
  }
}
