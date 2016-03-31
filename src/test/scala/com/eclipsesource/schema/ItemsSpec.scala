package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class ItemsSpec extends Specification with JsonSpec {
  validate("items")


  "validate array with some invalid items" in {
    val s = JsonSource.schemaFromString(
      """{
        |"type": "array",
        |"items": {
        | "minimum": 3
        |}
        |}""".stripMargin).get

    val i = Json.arr(2, 3, 4, 1)

    val result = SchemaValidator.validate(s)(i)
    result.isFailure must beTrue
    result.asEither must beLeft.like { case error =>
      error.toJson.value must haveLength(2)
    }
  }
}

