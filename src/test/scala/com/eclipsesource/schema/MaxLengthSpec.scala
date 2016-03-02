package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json._

class MaxLengthSpec extends Specification with JsonSpec {
  validate("maxLength")

  "MaxLength" should {
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
