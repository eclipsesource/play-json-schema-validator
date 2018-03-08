package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.{JsArray, JsNumber}

class AdditionalItemsSpec extends Specification with JsonSpec {

  "validate draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("additionalItems", "draft4")
  }

  "validate draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("additionalItems", "draft7")
  }

  "AdditionalItems" should {
    import Version7._
    val schema = JsonSource.schemaFromString(
      """{
        |  "items": [{}, {}, {}],
        |  "additionalItems": false
        |}""".stripMargin).get

    "no additional items present" in {
      val data = JsArray(Seq(JsNumber(1), JsNumber(2), JsNumber(3)))
      SchemaValidator(Some(Version4)).validate(schema, data).isSuccess must beTrue
    }
  }
}
