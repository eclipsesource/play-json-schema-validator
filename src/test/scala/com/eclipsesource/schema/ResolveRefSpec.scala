package com.eclipsesource.schema.internal

import com.eclipsesource.schema.test.JSONSource
import com.eclipsesource.schema.{SchemaInteger, SchemaArray, SchemaType}
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.data.mapping.Path
import play.api.libs.json._
import play.api.mvc.Handler
import play.api.test.{FakeApplication, WithServer}

class ResolveRefSpec extends Specification {

  "Relative ref" should {

    val schema = JSONSource.schemaFromString(
      """{
        |"$ref": "http://json-schema.org/draft-04/schema#"
                }""".stripMargin).get

    "be resolvable via " in {

      val context = Context(Path, schema, Seq.empty, Set.empty)
      val updatedRoot = RefResolver.replaceRefs(context)(schema)
      val resolved: Option[SchemaType] = RefResolver.resolveRef("#/definitions/schemaArray", context.copy(root = updatedRoot))
      resolved must beSome.which(t => t.isInstanceOf[SchemaArray])
    }

    "resolve ref" in {
      val context = Context(Path, schema, Seq.empty, Set.empty)
      val updatedRoot = RefResolver.replaceRefs(context)(schema)
      val resolved = RefResolver.resolveRef("#/properties/anyOf", context.copy(root = updatedRoot))
      resolved must beSome.which(t => t.isInstanceOf[SchemaArray])
    }

  }

}
