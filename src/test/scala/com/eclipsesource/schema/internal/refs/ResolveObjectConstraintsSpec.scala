package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.{JsonSource, SchemaString, SchemaType}
import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.draft7.constraints.StringConstraints7
import org.specs2.mutable.Specification
import play.api.libs.json.{JsBoolean, JsObject, JsString, JsValue}

class ResolveObjectConstraintsSpec extends Specification {

  "draft v4" should {

    import Version4._
    val resolver = SchemaRefResolver(Version4)

    "resolve dependencies constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"dependencies": {
          |    "a": "b",
          |    "c": ["d", "e"]
          |  }
          |}""".stripMargin).get

      val resolved = resolver.resolveFromRoot("#/dependencies/c/1", SchemaResolutionScope(schema))
      resolved.right.map(_.toJson) must beRight[JsValue](JsString("e"))
    }

    "resolve patternProperties constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "patternProperties": {
          |        "^(/[^/]+)+$": {}
          |  }
          |}""".stripMargin).get
      val result = resolver.resolveFromRoot("#/patternProperties", SchemaResolutionScope(schema))
      result.map(_.toJson).right.get must beAnInstanceOf[JsObject]
    }

    "should resolve additionalProperties constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "additionalProperties": false
          |}""".stripMargin).get

      val result = resolver.resolveFromRoot("#/additionalProperties", SchemaResolutionScope(schema))
      result.map(_.toJson) must beRight[JsValue](JsBoolean(false))
    }
  }

  "draft v7" should {

    import Version7._
    val resolver = SchemaRefResolver(Version7)

    "resolve dependencies constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"dependencies": {
          |    "a": "b",
          |    "c": ["d", "e"]
          |  }
          |}""".stripMargin).get

      val resolved = resolver.resolveFromRoot("#/dependencies/c/1", SchemaResolutionScope(schema))
      resolved.right.map(_.toJson) must beRight[JsValue](JsString("e"))
    }

    "resolve patternProperties constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "patternProperties": {
          |        "^(/[^/]+)+$": {}
          |  }
          |}""".stripMargin).get
      val result = resolver.resolveFromRoot("#/patternProperties", SchemaResolutionScope(schema))
      result.map(_.toJson).right.get must beAnInstanceOf[JsObject]
    }

    "should resolve additionalProperties constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "additionalProperties": false
          |}""".stripMargin).get

      val result = resolver.resolveFromRoot("#/additionalProperties", SchemaResolutionScope(schema))
      result.map(_.toJson) must beRight[JsValue](JsBoolean(false))
    }

    "should resolve additionalProperties constraint" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "propertyNames": {"maxLength": 3}
          |}""".stripMargin).get

      val result = resolver.resolveFromRoot("#/propertyNames", SchemaResolutionScope(schema))
      result.map(_.resolved) must beRight[SchemaType](SchemaString(StringConstraints7().copy(maxLength = Some(3))))
    }
  }
}
