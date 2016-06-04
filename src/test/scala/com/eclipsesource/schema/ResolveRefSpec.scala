package com.eclipsesource.schema

import com.eclipsesource.schema.internal.{ResolutionScope, RefResolver}
import org.specs2.mutable.Specification

class ResolveRefSpec extends Specification {

  "Relative ref" should {

    val RefResolver = new RefResolver {}

    val schema = JsonSource.schemaFromString(
      """{
        |  "$ref": "http://json-schema.org/draft-04/schema#"
        |}""".stripMargin).get

    "be resolvable via " in {

      val context = ResolutionScope(schema)
      val resolved: Option[SchemaType] = RefResolver.resolve("#/definitions/schemaArray", context)
      resolved must beSome.which(t => t.isInstanceOf[SchemaArray])
    }

    "resolve ref" in {
      val context = ResolutionScope(schema)
      val resolved = RefResolver.resolve("#/properties/anyOf", context)
      resolved must beSome.which(t => t.isInstanceOf[SchemaArray])
    }
  }
}
