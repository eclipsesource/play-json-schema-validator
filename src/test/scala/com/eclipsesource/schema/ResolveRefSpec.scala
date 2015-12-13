package com.eclipsesource.schema

import com.eclipsesource.schema.internal.{Context, RefResolver}
import org.specs2.mutable.Specification
import play.api.data.mapping.Path


class ResolveRefSpec extends Specification {

  "Relative ref" should {

    val schema = JsonSource.schemaFromString(
      """{
        |  "$ref": "http://json-schema.org/draft-04/schema#"
        |}""".stripMargin).get

    "be resolvable via " in {

      val context = Context(schema)
      val resolved: Option[SchemaType] = RefResolver.resolve("#/definitions/schemaArray", context)
      resolved must beSome.which(t => t.isInstanceOf[SchemaArray])
    }

    "resolve ref" in {
      val context = Context(schema)
      val resolved = RefResolver.resolve("#/properties/anyOf", context)
      resolved must beSome.which(t => t.isInstanceOf[SchemaArray])
    }
  }
}
