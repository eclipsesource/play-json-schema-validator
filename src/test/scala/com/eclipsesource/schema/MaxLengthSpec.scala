package com.eclipsesource.schema

import com.eclipsesource.schema.internal.draft4.Version4
import com.eclipsesource.schema.internal.draft7.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json._

class MaxLengthSpec extends Specification with JsonSpec {

  "maxLength draft4" in {
    import Version4._
    implicit val validator = SchemaValidator(Version4)
    validate("maxLength")
  }

  "maxLength draft7" in {
    import Version7._
    implicit val validator = SchemaValidator(Version7)
    validate("maxLength", "draft7")
  }

  "MaxLength" should {
    import com.eclipsesource.schema.internal.draft4.Version4._
    "fail with an error in case the string is too long" in {
      val schema = JsonSource.schemaFromString(
        """{
          |"maxLength": 2
        }""".stripMargin).get
      val json = JsString("foo")
      val result = SchemaValidator(Version4).validate(schema)(json)
      result.isError must beTrue
      result.asEither must beLeft.like {
        case error => (error.toJson(0) \ "msgs") == JsDefined(JsArray(Seq(JsString("'foo' exceeds maximum length of 2."))))
      }
    }
  }
}
