package com.eclipsesource.schema

import com.eclipsesource.schema.internal.validators.DefaultFormats
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
        obj \ "msgs" == JsDefined(JsArray(Seq(JsString(s"Unknown format '$formatName'."))))
      }
    }

    "validate UUIDs" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "format": "uuid"
          | }
        """.stripMargin).get
      val validator = SchemaValidator()
      validator.validate(schema, JsString("6a12a4d5-e9e6-4568-afcc-34c70b24a668")).isSuccess must beTrue
      validator.validate(schema, JsString("foo")).isError must beTrue
    }

    "validate custom formats" in {
      val lowerCaseOnlyFormat = new SchemaStringFormat {
        override def name: String = "my-format"
        override def validate(s: String): Boolean = s.filterNot(_.isWhitespace).forall(_.isLower)
      }
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "format": "my-format"
          | }
        """.stripMargin).get
      val validator = SchemaValidator().addFormat(lowerCaseOnlyFormat)
      validator.validate(schema, JsString("this is all valid")).isSuccess must beTrue
      validator.validate(schema, JsString("Invalid")).isError must beTrue
    }
  }

}
