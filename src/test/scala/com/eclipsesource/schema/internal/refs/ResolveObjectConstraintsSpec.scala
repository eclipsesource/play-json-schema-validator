package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.JsonSource
import com.eclipsesource.schema.internal.draft4.Version4
import org.specs2.mutable.Specification
import play.api.libs.json.{JsBoolean, JsObject, JsString, JsValue}

class ResolveObjectConstraintsSpec extends Specification {

  "SchemaRefResolver" should {

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
}
