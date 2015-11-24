package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema.{SchemaBoolean, SchemaType, JsonSource}
import org.specs2.mutable.Specification
import play.api.libs.json.{Json, JsString}

class SchemaWritesSpec extends Specification {

  "Writes" should {
    "serialize string contraints" in {

      val stringConstraint = """{
        |"maxLength": 2
      }""".stripMargin

      val schema: SchemaType = JsonSource.schemaFromString(stringConstraint).get
      Json.toJson(schema) must beEqualTo(Json.obj(
        "type" -> "string",
        "maxLength" -> 2
      ))
    }

    "serialize boolean constraints" in {
      Json.toJson(SchemaBoolean()) must beEqualTo(Json.obj(
        "type" -> "boolean"
      ))
    }
  }


}
