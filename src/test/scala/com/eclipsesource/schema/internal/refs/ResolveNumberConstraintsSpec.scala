package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.{JsonSource, SchemaType, SchemaValue}
import org.specs2.mutable.Specification
import play.api.libs.json.JsNumber

class ResolveNumberConstraintsSpec extends Specification {

  "SchemaRefResolver" should {

    import Version4._
    val resolver = SchemaRefResolver(Version4)

    "resolve number constraints" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "integer",
          |  "minimum": 0,
          |  "maximum": 10,
          |  "multipleOf": 2
          |}""".stripMargin).get

      val scope = SchemaResolutionScope(schema)
      resolver.resolveFromRoot("#/minimum", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(0)))
      resolver.resolveFromRoot("#/maximum", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(10)))
      resolver.resolveFromRoot("#/multipleOf", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(2)))
    }
  }
}
