package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.drafts.Version4
import com.eclipsesource.schema.{JsonSource, SchemaType, SchemaValue}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsBoolean, JsNumber}

class ResolveArrayConstraintsSpec extends Specification {

  "SchemaRefResolver" should {

    import Version4._
    val resolver = SchemaRefResolver(Version4)

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

      val scope = SchemaResolutionScope(schema)
      resolver.resolveFromRoot("#/minItems", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(42)))
      resolver.resolveFromRoot("#/maxItems", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(99)))
      resolver.resolveFromRoot("#/additionalItems", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsBoolean(false)))
      resolver.resolveFromRoot("#/uniqueItems", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsBoolean(false)))
    }
  }
}
