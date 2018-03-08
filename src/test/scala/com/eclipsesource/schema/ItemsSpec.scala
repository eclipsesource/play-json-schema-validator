package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.refs.{SchemaRefResolver, SchemaResolutionScope}
import com.eclipsesource.schema.internal.validators.ArrayValidator
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, Json}

class ItemsSpec extends Specification with JsonSpec {

  "validate draft4" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    validate("items", "draft4")
  }

  "validate draft7" in {
    import Version7._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    validate("items", "draft7")
  }

  "validate array with some invalid items" in {
    import Version4._
    implicit val validator: SchemaValidator = SchemaValidator(Some(Version4))
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "array",
        |  "items": {
        |    "minimum": 3
        |  }
        |}""".stripMargin).get

    val instance = Json.arr(2, 3, 4, 1)

    val result = validator.validate(schema, instance)
    result.isError must beTrue
    result.asEither must beLeft.like { case error =>
      error.toJson.value must haveLength(2)
    }
  }

  "validate array with wrong json type" in {
    import Version4._
    val schema = JsonSource.schemaFromString(
      """{
        |  "type": "array",
        |  "items": {
        |    "minimum": 3
        |  }
        |}""".stripMargin).get.asInstanceOf[SchemaArray]
    val context = SchemaResolutionContext(SchemaRefResolver(Version4), SchemaResolutionScope(schema))
    val result = ArrayValidator.validate(schema, JsNumber(2), context)
    result.isFailure must beTrue
  }
}

