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
      val lowerCaseOnlyFormat = new SchemaFormat {
        override def name: String = "my-format"
        override def validate(json: JsValue): Boolean = json match {
          case JsString(s) => s.filterNot(_.isWhitespace).forall(_.isLower)
          case _ => false
        }
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

    "validate int32 format" in {
      val integerSchema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "integer",
          |  "format": "int32"
          | }
        """.stripMargin).get
      val numberSchema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "number",
          |  "format": "int32"
          | }
        """.stripMargin).get

      validator.validate(integerSchema, JsNumber(Integer.MAX_VALUE)).isSuccess must beTrue
      validator.validate(integerSchema, JsNumber(BigDecimal.valueOf(Integer.MAX_VALUE) + 1)).isError must beTrue

      validator.validate(numberSchema, JsNumber(Integer.MAX_VALUE)).isSuccess must beTrue
      validator.validate(numberSchema, JsNumber(BigDecimal.valueOf(Integer.MAX_VALUE) + 1)).isError must beTrue
    }


    "validate int64 format" in {
      val numberSchema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "number",
          |  "format": "int64"
          | }
        """.stripMargin).get
      val integerSchema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "integer",
          |  "format": "int64"
          | }
        """.stripMargin).get

      validator.validate(integerSchema, JsNumber(Long.MaxValue)).isSuccess must beTrue
      validator.validate(integerSchema, JsNumber(BigDecimal.valueOf(Long.MaxValue) + 1)).isError must beTrue
      validator.validate(numberSchema, JsNumber(Long.MaxValue)).isSuccess must beTrue
      validator.validate(numberSchema, JsNumber(BigDecimal.valueOf(Long.MaxValue) + 1)).isError must beTrue
    }

    "validate custom number range format" in {
      val rangeFormat = new SchemaFormat {
        override def name: String = "range0To10"
        override def validate(json: JsValue): Boolean = json match {
          case JsNumber(n) => n >= 0 && n <= 10
          case _ => false
        }
      }
      val validatorWithRangeFormat = validator.addFormat(rangeFormat)
      val numberSchema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "number",
          |  "format": "range0To10"
          | }
        """.stripMargin).get

      val integerSchema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "integer",
          |  "format": "range0To10"
          | }
        """.stripMargin).get

      validatorWithRangeFormat.validate(integerSchema, JsNumber(10)).isSuccess must beTrue
      validatorWithRangeFormat.validate(integerSchema, JsNumber(-1)).isError must beTrue

      validatorWithRangeFormat.validate(numberSchema, JsNumber(10)).isSuccess must beTrue
      validatorWithRangeFormat.validate(numberSchema, JsNumber(-1)).isError must beTrue
    }
  }
}
