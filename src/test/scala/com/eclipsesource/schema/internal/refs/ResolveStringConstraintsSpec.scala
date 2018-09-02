package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.{JsonSource, SchemaType, SchemaValue}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsString}

class ResolveStringConstraintsSpec extends Specification {

  "draft v4" should {

    import Version4._
    val resolver = SchemaRefResolver(Version4)

    "resolve string constraints" in {
      val schema =
        JsonSource.schemaFromString(
          """{
            |  "type": "string",
            |  "minLength": 1,
            |  "maxLength": 10,
            |  "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
            |}""".stripMargin).get

      val scope = SchemaResolutionScope(schema)
      resolver.resolveFromRoot("#/minLength", scope)
        .map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(1)))
      resolver.resolveFromRoot("#/maxLength", scope)
        .map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(10)))
      resolver.resolveFromRoot("#/pattern", scope)
        .map(_.resolved) must beRight[SchemaType](SchemaValue(JsString("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")))
    }
  }

  "draft v7" should {

    import Version7._
    val resolver = SchemaRefResolver(Version7)

    "resolve string constraints" in {
      val schema =
        JsonSource.schemaFromString(
          """{
            |  "type": "string",
            |  "minLength": 1,
            |  "maxLength": 10,
            |  "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
            |}""".stripMargin).get

      val scope = SchemaResolutionScope(schema)
      resolver.resolveFromRoot("#/minLength", scope)
        .map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(1)))
      resolver.resolveFromRoot("#/maxLength", scope)
        .map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(10)))
      resolver.resolveFromRoot("#/pattern", scope)
        .map(_.resolved) must beRight[SchemaType](SchemaValue(JsString("^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$")))
    }
  }
}
