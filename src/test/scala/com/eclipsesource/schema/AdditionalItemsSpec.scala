package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JsonSpec}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsArray}

class AdditionalItemsSpec extends Specification with JsonSpec {

  validate("additionalItems")

  "AdditionalItems" should {
    val schema = JsonSource.schemaFromString(
      """{
        |  "items": [{}, {}, {}],
        |  "additionalItems": false
        |}""".stripMargin).get

    "no additional items present" in {
      val data = JsArray(Seq(JsNumber(1), JsNumber(2), JsNumber(3)))
      SchemaValidator().validate(schema, data).isSuccess must beTrue
    }
  }

}
