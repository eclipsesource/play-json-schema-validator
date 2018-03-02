package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.constraints.Constraints.{Maximum, Minimum}
import com.eclipsesource.schema.internal.draft4.constraints.{AnyConstraints4, ArrayConstraints4, NumberConstraints4, ObjectConstraints4}
import com.eclipsesource.schema.internal.refs.Ref
import org.specs2.mutable.Specification
import play.api.libs.json.{JsBoolean, JsString, Json}

class SchemaWritesSpec extends Specification {

  "Schema Writes for draft 4" should {

    import com.eclipsesource.schema.internal.draft4.Version4._

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
      Json.toJson(SchemaBoolean(AnyConstraints4())) must beEqualTo(Json.obj(
        "type" -> "boolean"
      ))
    }

    "write integer" in {
      Json.toJson(SchemaInteger(NumberConstraints4())) must beEqualTo(Json.obj(
        "type" -> "integer"
      ))
    }

    "write number" in {
      Json.toJson(
        SchemaNumber(
          NumberConstraints4(Some(Minimum(2, Some(false))), Some(Maximum(10, Some(false))), Some(2))
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
      Json.toJson(SchemaArray(SchemaNumber(NumberConstraints4()), ArrayConstraints4())) must beEqualTo(Json.obj(
        "items" -> Json.obj("type" -> "number")
      ))
    }

    "write tuple" in {
      Json.toJson(SchemaTuple(Seq(SchemaNumber(NumberConstraints4())), ArrayConstraints4())) must beEqualTo(Json.obj(
        "items" -> Json.arr(Json.obj("type" -> "number"))
      ))
    }

    "write $ref" in {
      Json.toJson(
        SchemaRef(Ref("#"), AnyConstraints4())
      ) must beEqualTo(
        Json.obj(
          "$ref" -> "#"
        )
      )
    }

    "write compound type" in {
      Json.toJson(CompoundSchemaType(Seq(SchemaNumber(NumberConstraints4()), SchemaBoolean(AnyConstraints4())))) must beEqualTo(Json.obj(
        "type" -> Json.arr("number", "boolean")
      ))
    }

    "write schema with definitions block" in {
      Json.toJson(
        SchemaNumber(
          NumberConstraints4().copy(any = AnyConstraints4().copy(definitions = Some(Map(
            "foo" -> SchemaNumber(NumberConstraints4().copy(min = Some(Minimum(BigDecimal(3), Some(false))))
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

  "Schema Writes for draft 7" should {

    import com.eclipsesource.schema.internal.draft7.Version7._

    "write boolean schema" in {
      val schema: SchemaType = SchemaValue(JsBoolean(true))
      Json.toJson(schema) must beEqualTo(JsBoolean(true))
    }
  }
}
