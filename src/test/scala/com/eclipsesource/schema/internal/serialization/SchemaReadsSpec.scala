package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.{JsonSource, SchemaType, SchemaValue}
import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.draft7.constraints.{AnyConstraints7, NumberConstraints7, StringConstraints7}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}

class SchemaReadsSpec extends Specification {


  "Schema Reads for draft 4" should {

    import Version4._

    "not fail with match error (#104)" in {
      val schema = JsonSource.schemaFromString("""
        |{
        |  "someProp": {"type": "sting"}
        |}""".stripMargin)

      schema.isSuccess must beTrue
    }

    "not be able to read boolean schema" in {
      Json.fromJson[SchemaType](JsBoolean(true)).isError must beTrue
    }

    "fail if exclusiveMinimum is not a boolean" in {
      val result = JsonSource.schemaFromString(
        """{ "exclusiveMinimum": 3 }""".stripMargin
      )
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "msgs").get.as[JsArray].value.head ==
        JsString("error.expected.jsboolean")
      }
    }
  }

  "Schema Reads for draft 7" should {

    import Version7._

    "read boolean schema" in {
      val booleanSchema = Json.fromJson[SchemaType](JsBoolean(true)).get
      booleanSchema must beEqualTo(SchemaValue(JsBoolean(true)))
    }

    "read compound type with error" in {
      val schema = JsonSource.schemaFromString("""
                                                 |{
                                                 |  "type": ["number", "string"]
                                                 |}""".stripMargin)

      schema.isSuccess must beTrue
      schema.asOpt must beSome.which(
        _ == CompoundSchemaType(
          Seq(
            SchemaNumber(NumberConstraints7().copy(any = AnyConstraints7().copy(schemaType = Some("number")))),
            SchemaString(StringConstraints7().copy(any = AnyConstraints7().copy(schemaType = Some("string"))))
          )
        )
      )
    }
  }
}
