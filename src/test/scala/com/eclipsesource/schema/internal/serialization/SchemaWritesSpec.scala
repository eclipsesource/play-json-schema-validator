package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.constraints.Constraints.{AnyConstraint, Maximum, Minimum, NumberConstraints}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsString, Json}

class SchemaWritesSpec extends Specification {

  import Version4._

  "JSON Schema Writes" should {

    "write string" in {
      val stringConstraint = """{
        |"maxLength": 2
      }""".stripMargin

      val schema: SchemaType = JsonSource.schemaFromString(stringConstraint).get
      Json.toJson(schema) must beEqualTo(Json.obj(
        "maxLength" -> 2
      ))
    }

    "write string with explicit type given" in {
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

    "write boolean" in {
      Json.toJson(SchemaBoolean()) must beEqualTo(Json.obj(
        "type" -> "boolean"
      ))
    }

    "write integer" in {
      Json.toJson(SchemaInteger()) must beEqualTo(Json.obj(
        "type" -> "integer"
      ))
    }

    "write number" in {
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

    "write array" in {
      Json.toJson(SchemaArray(SchemaNumber())) must beEqualTo(Json.obj(
        "items" -> Json.obj("type" -> "number")
      ))
    }

    "write tuple" in {
      Json.toJson(SchemaTuple(Seq(SchemaNumber()))) must beEqualTo(Json.obj(
        "items" -> Json.arr(Json.obj("type" -> "number"))
      ))
    }

    "write $ref" in {
      Json.toJson(SchemaObject(Seq(SchemaProp("$ref", SchemaValue(JsString("#")))))) must beEqualTo(
        Json.obj(
          "$ref" -> "#"
        )
      )
    }

    "write compound type" in {
      Json.toJson(CompoundSchemaType(Seq(SchemaNumber(), SchemaBoolean()))) must beEqualTo(Json.obj(
        "type" -> Json.arr("number", "boolean")
      ))
    }

    "write schema with definitions block" in {
      implicit val version = Version4
      Json.toJson(
        SchemaNumber(
          NumberConstraints().copy(any = AnyConstraint().copy(definitions = Some(Map(
            "foo" -> SchemaNumber(NumberConstraints().copy(min = Some(Minimum(BigDecimal(3), Some(false))))
            ))))
          )
        )
      ) must beEqualTo(
        Json.obj(
          "definitions" -> Json.obj(
            "foo" -> Json.obj(
              "minimum" -> 3,
              "exclusiveMinimum" -> false
            )
          )
        )
      )
    }
  }
}
