package com.eclipsesource.schema

import com.eclipsesource.schema.internal.{RefResolver, ResolutionContext, ResolutionScope}
import com.eclipsesource.schema.internal.validators.ArrayValidator
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, Json}

class ItemsSpec extends Specification with JsonSpec {
  validate("items")

  "validate array with some invalid items" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "array",
        |  "items": {
        |    "minimum": 3
        |  }
        |}""".stripMargin).get

    val instance = Json.arr(2, 3, 4, 1)

    val result = SchemaValidator().validate(schema, instance)
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      error.toJson.value must haveLength(2)
    }
  }

  "validate array with wrong json type" in {
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "array",
        |  "items": {
        |    "minimum": 3
        |  }
        |}""".stripMargin).get.asInstanceOf[SchemaArray]
    val context = new ResolutionContext(new RefResolver, new ResolutionScope(schema))
    val result = ArrayValidator.validate(schema, JsNumber(2), context)
    result.isFailure must beTrue
  }
}

