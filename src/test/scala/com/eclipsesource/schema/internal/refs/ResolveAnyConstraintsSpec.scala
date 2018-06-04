package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum}
import com.eclipsesource.schema.internal.draft7.constraints.NumberConstraints7
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.eclipsesource.schema.{JsonSource, SchemaArray, SchemaConfigOptions, SchemaFormat, SchemaNumber, SchemaType}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsValue, Json}

class ResolveAnyConstraintsSpec extends Specification { self =>

  "draft v4" should {

    import Version4._
    val talkSchema = JsonSource.schemaFromString("""{
      "$schema": "http://json-schema.org/draft-04/schema#",
      "type": "object",
      "properties": {
        "title": {
        "type": "string",
        "minLength": 10,
        "maxLength": 20
      },
        "speaker": {
        "type": "string",
        "pattern": "(Mr.|Mrs.)?[A-Za-z ]+"
      },
        "type": { "$ref": "talk_type.json" },
        "date": { "$ref": "date.json" },
        "location": { "$ref": "location.json" }
      }
    }""").get

    val resolver = SchemaRefResolver(Version4)
      .copy(cache = DocumentCache()
        .add(Ref(Version4.SchemaUrl))(Version4.Schema)
        .add(Ref("http://localhost:1234/talk.json#"))(talkSchema)
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
      arrayDef.map(_.resolved) must beRight[SchemaType].which(_.isInstanceOf[SchemaArray])
      anyOf.map(_.resolved) must beRight[SchemaType].which(_.isInstanceOf[SchemaArray])
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

  "draft v7" should {

    import Version7._

    val talkSchema = JsonSource.schemaFromString("""{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "object",
      "properties": {
        "title": {
        "type": "string",
        "minLength": 10,
        "maxLength": 20
      },
        "speaker": {
        "type": "string",
        "pattern": "(Mr.|Mrs.)?[A-Za-z ]+"
      },
        "type": { "$ref": "talk_type.json" },
        "date": { "$ref": "date.json" },
        "location": { "$ref": "location.json" }
      }
    }""").get

    val resolver = SchemaRefResolver(
      Version7(new SchemaConfigOptions {
        val supportsExternalReferences = true
        def formats: Map[String, SchemaFormat] = DefaultFormats.formats
      }))
      .copy(cache = DocumentCache()
        .add(Ref(Version7.SchemaUrl))(JsonSource.schemaFromUrl(self.getClass.getResource("/json-schema-draft-07.json")).get)
        .add(Ref("http://localhost:1234/talk.json#"))(talkSchema)
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


    "resolve references into JSON schema v7 draft" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "$ref": "http://json-schema.org/draft-07/schema#"
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

    "resolve if/then/else" in {
      val schema = JsonSource.schemaFromString("""{
        |"if": {
        |  "exclusiveMaximum": 0
        |},
        |"then": {
        |  "minimum": -10
        |},
        |"else": {
        |  "multipleOf": 2
        |}
      }""".stripMargin).get
      resolver.resolveFromRoot("#/if", SchemaResolutionScope(schema))
        .map(_.resolved) must beRight[SchemaType](SchemaNumber(NumberConstraints7().copy(max = Some(Maximum(0, Some(true))))))
      resolver.resolveFromRoot("#/then", SchemaResolutionScope(schema))
        .map(_.resolved) must beRight[SchemaType](SchemaNumber(NumberConstraints7().copy(min = Some(Minimum(-10, Some(false))))))
      resolver.resolveFromRoot("#/else", SchemaResolutionScope(schema))
        .map(_.resolved) must beRight[SchemaType](SchemaNumber(NumberConstraints7().copy(multipleOf = Some(BigDecimal(2)))))
    }
  }
}
