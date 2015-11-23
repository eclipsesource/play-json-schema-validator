package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JsonSpec}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsArray, Json}

class AdditionalItemsSpec extends Specification {

  "AdditionalItems" should {

    val resourceUrl: URL = getClass.getResource("/draft4/additionalItems.json")

    "validate" in {
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(_.execute)
    }

    val schema = JsonSource.schemaFromString(
      """{
        |  "items": [{}, {}, {}],
        |  "additionalItems": false
        |}""".stripMargin).get

    "no additional items present" in {
      val data = JsArray(Seq(JsNumber(1), JsNumber(2), JsNumber(3)))
      SchemaValidator.validate(schema, data).isSuccess must beTrue
    }
  }

}
