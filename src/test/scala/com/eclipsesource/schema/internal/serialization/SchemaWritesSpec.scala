package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum, NumberConstraints}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsString, Json}

class SchemaWritesSpec extends Specification {

  "JSON Schema Writes" should {

    "serialize string" in {
      val stringConstraint = """{
        |"maxLength": 2
      }""".stripMargin

      val schema: SchemaType = JsonSource.schemaFromString(stringConstraint).get
      Json.toJson(schema) must beEqualTo(Json.obj(
        "maxLength" -> 2
      ))
    }

    "serialize string with explicit type given" in {

      val stringConstraint = """{
       |"type": "string",
       |"maxLength": 2
      }""".stripMargin

      val schema: SchemaType = JsonSource.schemaFromString(stringConstraint).get
      Json.toJson(schema) must beEqualTo(Json.obj(
        "type" -> "string",
        "maxLength" -> 2
      ))
    }

    "serialize boolean" in {
      Json.toJson(SchemaBoolean()) must beEqualTo(Json.obj(
        "type" -> "boolean"
      ))
    }

    "serialize integer" in {
      Json.toJson(SchemaInteger()) must beEqualTo(Json.obj(
        "type" -> "integer"
      ))
    }

    "serialize number" in {
      Json.toJson(
        SchemaNumber(
          NumberConstraints(Some(Minimum(2, Some(false))), Some(Maximum(10, Some(false))), Some(2))
        )
      ) must beEqualTo(Json.obj(
        "minimum" -> 2,
        "exclusiveMinimum" -> false,
        "maximum" -> 10,
        "exclusiveMaximum" -> false,
        "multipleOf" -> 2
      ))
    }

    "serialize array" in {
      Json.toJson(SchemaArray(SchemaNumber())) must beEqualTo(Json.obj(
        "items" -> Json.obj("type" -> "number")
      ))
    }

    "serialize tuple" in {
      Json.toJson(SchemaTuple(Seq(SchemaNumber()))) must beEqualTo(Json.obj(
        "items" -> Json.arr(Json.obj("type" -> "number"))
      ))
    }

    "serialize $ref" in {
      Json.toJson(SchemaObject(Seq(SchemaAttribute("$ref", SchemaValue(JsString("#")))))) must beEqualTo(
        Json.obj(
          "$ref" -> "#"
        )
      )
    }

    "compound type" in {
      Json.toJson(CompoundSchemaType(Seq(SchemaNumber(), SchemaBoolean()))) must beEqualTo(Json.obj(
        "type" -> Json.arr("number", "boolean")
      ))
    }
  }
}
