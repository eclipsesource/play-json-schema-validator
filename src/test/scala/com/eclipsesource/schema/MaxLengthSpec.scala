package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.{JsArray, JsDefined, JsString}

class MaxLengthSpec extends Specification {

  "MaxLength" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/maxLength.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }

    "fail with an error in case the string is too long" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"maxLength": 2
        }""".stripMargin).get
      val json = JsString("foo")
      val result = SchemaValidator.validate(schema)(json)
      result.isFailure must beTrue
      result.asEither must beLeft.like { case error => (error.toJson(0) \ "msgs") == JsDefined(JsArray(Seq(JsString("foo violates max length of 2")))) }
    }
  }

}
