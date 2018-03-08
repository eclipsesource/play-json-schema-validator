package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version4
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.JsNumber

class BigNumSpec extends Specification with JsonSpec {

  import Version4._
  implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
  validate("optional/bignum", "draft4")

  "Bignum" should {

    "be an integer" in {
      val schema = JsonSource.schemaFromString(""" {"type": "integer"} """).get
      val instance = JsNumber(BigDecimal("12345678910111213141516171819202122232425262728293031"))
      val result = SchemaValidator(Some(Version4)).validate(schema)(instance)
      result.asOpt must beSome.which(_ == JsNumber(BigDecimal("12345678910111213141516171819202122232425262728293031")))
    }

  }
}
