package com.eclipsesource.schema

import org.specs2.mutable.Specification
import com.eclipsesource.schema._
import play.api.libs.json.{JsString, JsNumber, JsResult, Json}

class MaximumSpec extends Specification {

  "maximum validation" should {

    val jsonSchema = Json.obj("maximum" -> 3.0)
    val schema: JsResult[SchemaObject] = Json.fromJson(jsonSchema)(valueReader).asInstanceOf[JsResult[SchemaObject]]

    "below the maximum is valid" in {
      val data = JsNumber(2.6)
      Validator.validate(schema.get)(data).isSuccess must beTrue
    }

    "below the maximum is valid" in {
      val data = JsNumber(3.5)
      Validator.validate(schema.get)(data).isFailure  must beTrue
    }

    "ignores non-numbers" in {
      val data = JsString("x")
      Validator.validate(schema.get)(data).isSuccess must beTrue
    }
  }

  "exclusiveMaximum validation" should {

    val jsonSchema = Json.obj(
      "maximum" -> 3.0,
      "exclusiveMaximum" -> true
    )

    val schema: JsResult[SchemaObject] = Json.fromJson(jsonSchema)(valueReader).asInstanceOf[JsResult[SchemaObject]]

    "below the maximum is still valid" in {
      val data = JsNumber(2.2)
      Validator.validate(schema.get)(data).isSuccess must beTrue
    }

    "boundary point is invalid" in {
      val data = JsNumber(3.0)
      Validator.validate(schema.get)(data).isFailure must beTrue
    }


  }

}
