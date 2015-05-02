package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json._
import shapeless.{HNil, Nat}

class ReferenceSpec extends Specification {

  // TODO: remvoe import

  sequential

  "Root pointer ref" should {

    val schema: QBClass = obj(
      "properties" -> obj(
        "foo" -> optional($ref("#"))
      )
    )

//    println(schema.prettyPrint)

//    val schemaAsString = """{
//      "type" : "object",
//      "properties" : {
//        "foo" : {
//          "$ref" : "#"
//        },
//        "o": {
//          "type" : "object",
//          "properties": {
//            "i" : { "type": "integer" }
//          }
//        }
//      }
//    }"""

//    val readSchema: JsResult[QBClass] = Json.fromJson(Json.parse(schemaAsString))(objectReader)

    println(Json.prettyPrint(Json.toJson(schema)))
//    println(readSchema)
//    val r = readSchema.get
//    println(r.prettyPrint)
//    println("!" + Json.prettyPrint(Json.toJson(r)))

    "match" in {
      val data = Json.obj(
        "foo" -> false
      )
      Validator.validate(schema)(data).isSuccess must beTrue
    }

    "recursive match" in {
      val data = Json.obj(
        "foo" -> Json.obj(
          "foo" -> false
        )
      )
      Validator.validate(schema)(data).isSuccess must beTrue
    }

    "mismatch" in {
      val data = Json.obj(
        "bar" -> false
      )
      Validator.validate(schema)(data).isFailure must beTrue
    }

    "recursive mismatch" in {
      val data = Json.obj(
        "foo" -> Json.obj(
          "bar" -> false
        )
      )
      Validator.validate(schema)(data).isFailure must beTrue
    }
  }

  "Relative pointer ref to object" should {
    val schema = obj(
      "properties" -> obj(
        "foo" -> optional(qbInteger),
        "bar" -> optional($ref("#/properties/foo"))
      )
    )

    println(Json.prettyPrint(Json.toJson(schema)))

    "match" in {
      val data = Json.obj(
        "bar" -> 3
      )
      val validationResult = Validator.validate(schema)(data)
      println(validationResult)
      validationResult.isSuccess must beTrue
    }

    "mismatch" in {
      val data = Json.obj(
        "bar" -> true
      )
      Validator.validate(schema)(data).isFailure must beTrue
    }
  }

  "Relative pointer ref to array" in {
    val schema = obj(
      "items" -> tuple(
        qbInteger,
        $ref("#/items/0") // TODO: int parameter
      )
    )

    println(Json.prettyPrint(Json.toJson(schema)))

    "match array" in {
      val data = Json.arr(1, 2)
      println(schema)
      val result = Validator.validate(schema)(data)
      println(result)
      result.isSuccess must beTrue
    }

    "mismatch array" in {
      val data = Json.arr(1, "foo")
      Validator.validate(schema)(data).isFailure must beTrue
    }
  }

  "Escaped pointer ref" should {
    val schema = obj(
      "tilda~field" -> optional(qbInteger),
      "slash/field" -> optional(qbInteger),
      "percent%field" -> optional(qbInteger),
      "properties" -> obj(
        "tilda" -> optional(
          $ref("#/tilda~0field"))
        ,
        "slash" -> optional(
          $ref("#/slash~1field")
        ),
        "percent" -> optional(
          $ref("#/percent%25field")
        )
      )
    )

    println(Json.prettyPrint(Json.toJson(schema)))

    "slash" in {
      val data = Json.obj("slash" -> "aeou")
      val result = Validator.validate(schema)(data)
      result.isFailure must beTrue
    }

    "tilda" in {
      val data = Json.obj("tilda" -> "aeou")
      val result = Validator.validate(schema)(data)
      println(result)
      result.isFailure must beTrue
    }

    "percent" in {
      val data = Json.obj("percent" -> "aeou")
      val result = Validator.validate(schema)(data)
      println(result)
      result.isFailure must beTrue
    }
  }

  "nested refs" should {

    val schema = obj(
      "definitions" -> obj(
        "a" -> qbInteger,
        "b" -> $ref("#/definitions/a"),
        "c" -> $ref("#/definitions/b")
      ),
      "$ref" -> QBRef(JSONPointer("#/definitions/c"), None, true)
    )

    println(Json.prettyPrint(Json.toJson(schema)))

    "nested ref valid" in {
      val data: JsValue = JsNumber(5)
      val result = Validator.validate(schema)(data)
      println(result)
      result.isSuccess must beTrue
    }

    "nested ref invalid" in {
      val data: JsValue = JsString("a")
      val result = Validator.validate(schema)(data)
      println(result)
      result.isFailure must beTrue
    }

  }

  // TODO
//  "remote ref, containing refs itself"
}
