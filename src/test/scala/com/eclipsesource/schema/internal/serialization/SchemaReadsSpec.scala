package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema.{JsonSource, SchemaType, SchemaValue}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsBoolean, Json}

class SchemaReadsSpec extends Specification {


  "Schema Reads for draft 4" should {

    import com.eclipsesource.schema.internal.draft4.Version4._

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
  }

  "Schema Reads for draft 7" should {

    import com.eclipsesource.schema.internal.draft7.Version7._

    "read boolean schema" in {
      val booleanSchema = Json.fromJson[SchemaType](JsBoolean(true)).get
      booleanSchema must beEqualTo(SchemaValue(JsBoolean(true)))
    }
  }
}
