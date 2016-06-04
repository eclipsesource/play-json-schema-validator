package com.eclipsesource.schema

import org.specs2.mutable.Specification
import com.eclipsesource.schema.test.JsonSpec
import play.api.libs.json._

class FormatSpec extends Specification with JsonSpec {
  validate("optional/format")

  "Format" should {
    "not validate unknown format" in {
      val formatName = "unknown"
      val schema = JsonSource.schemaFromString(
        s"""{"format": "$formatName"}"""
      ).get
      val result = SchemaValidator().validate(schema, JsString("some string"))
      result.asEither must beLeft.like { case error =>
        val JsDefined(obj) = error.toJson(0)
        obj \ "msgs" == JsDefined(JsArray(Seq(JsString(s"Unknown format $formatName"))))
      }
    }

  }

}
