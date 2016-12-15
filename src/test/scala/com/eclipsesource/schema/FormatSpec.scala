package com.eclipsesource.schema

import org.specs2.mutable.Specification
import com.eclipsesource.schema.test.JsonSpec
import play.api.libs.json._

class FormatSpec extends Specification with JsonSpec {
  validate("optional/format")

  "Format" should {

    "validate unknown format" in {
      val schema = JsonSource.schemaFromString(
        s"""{"format": "unknown"}"""
      ).get
      val result = SchemaValidator().validate(schema, JsString("some string"))
      result.isSuccess must beTrue
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

    "validate regex format" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "format": "regex"
          | }
        """.stripMargin).get
      val validator = SchemaValidator()
      validator.validate(schema, JsString("'['")).isError must beTrue
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
