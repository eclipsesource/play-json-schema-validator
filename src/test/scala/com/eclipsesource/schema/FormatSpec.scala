package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version4
import com.eclipsesource.schema.internal.validators.DefaultFormats.DatetimeFormat
import org.specs2.mutable.Specification
import com.eclipsesource.schema.test.JsonSpec
import play.api.libs.json._

class FormatSpec extends Specification with JsonSpec {

  implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
  import Version4._
  validate("optional/format", "draft4")

  "Format" should {

    "validate unknown format" in {
      val schema = JsonSource.schemaFromString(
        s"""{"format": "unknown"}"""
      ).get
      val result = SchemaValidator(Some(Version4)).validate(schema, JsString("some string"))
      result.isSuccess must beTrue
    }

    "validate emails" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "format": "email"
          |}
        """.stripMargin).get
      val validator = SchemaValidator(Some(Version4))
      validator.validate(schema, JsString("a%d33@example.co.uk")).isSuccess must beTrue
      validator.validate(schema, JsString(".@.....")).isError must beTrue
    }

    "validate UUIDs" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type": "string",
          |  "format": "uuid"
          | }
        """.stripMargin).get
      val validator = SchemaValidator(Some(Version4))
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
      val validator = SchemaValidator(Some(Version4))
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
      val validator = SchemaValidator(Some(Version4)).addFormat(lowerCaseOnlyFormat)
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

      validator.validate(integerSchema, JsNumber(Int.MinValue)).isSuccess must beTrue
      validator.validate(integerSchema, JsNumber(Int.MaxValue)).isSuccess must beTrue
      validator.validate(integerSchema, JsNumber(BigDecimal.valueOf(Int.MaxValue) + 1)).isError must beTrue

      validator.validate(numberSchema, JsNumber(Int.MinValue)).isSuccess must beTrue
      validator.validate(numberSchema, JsNumber(Int.MaxValue)).isSuccess must beTrue
      validator.validate(numberSchema, JsNumber(BigDecimal.valueOf(Int.MaxValue) + 1)).isError must beTrue
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

      validator.validate(integerSchema, JsNumber(Long.MinValue)).isSuccess must beTrue
      validator.validate(integerSchema, JsNumber(Long.MaxValue)).isSuccess must beTrue
      validator.validate(integerSchema, JsNumber(BigDecimal.valueOf(Long.MaxValue) + 1)).isError must beTrue

      validator.validate(numberSchema, JsNumber(Long.MinValue)).isSuccess must beTrue
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

    "validate integer with int32 format against double max boundary" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type" : "object",
          |  "properties" : {
          |    "intprop" : {
          |      "type" : "integer",
          |      "format" : "int32"
          |    }
          |  }
          |}
        """.stripMargin).get
      val instance = Json.obj(
        "intprop" -> -1.7976931348623157E+308
      )
      validator.validate(schema, instance).isError must beTrue
    }

    "validate integer without format against double max boundary" in {
      val schema = JsonSource.schemaFromString(
        """
          |{
          |  "type" : "object",
          |  "properties" : {
          |    "intprop" : {
          |      "type" : "integer"
          |    }
          |  }
          |}
        """.stripMargin).get
      val instance = Json.obj(
        "intprop" -> -1.7976931348623157E+308
      )
      validator.validate(schema, instance).isSuccess must beTrue
    }

    "validate date-time format" in {

      def validateDate(date: String): Boolean =
        DatetimeFormat.validate(JsString(date))

      validateDate("2007-12-03T10:15:30")         must beFalse
      validateDate("2007-12-03 10:15:30+01:00")   must beFalse
      validateDate("2007-12-03 10:15:30")         must beFalse
      validateDate("2007-12-03 10:15")            must beFalse
      validateDate("2007-12-03")                  must beFalse
      validateDate("2007-12-03T10:15:30.1111111") must beFalse
      validateDate("99999-12-03T10:15:30+01:00")  must beFalse
      validateDate("-00000-12-03T10:15:30+01:00") must beFalse
      validateDate("2007-12-32T10:15:30+01:00")   must beFalse
      validateDate("2007-13-03T10:15:30+01:00")   must beFalse
      validateDate("2007-12-03T25:15:30+01:00")   must beFalse
      validateDate("2007-12-03T10:60:30+01:00")   must beFalse
      validateDate("2007-12-03T10:15:60+01:00")   must beFalse
      validateDate("2007-12-03T10:15:30+19:00")   must beFalse
      validateDate("2007-12-03T10:15:30-19:00")   must beFalse
      validateDate("2007-13-03T10:15:30+01:60")   must beFalse
      validateDate("2007-12-03T10:15:30+20:00")   must beFalse
      validateDate("2007-12-03T10:15:30+01:00.1") must beFalse
      validateDate("2007:12:03T10:15:30+01:00.1") must beFalse
    }
  }
}
