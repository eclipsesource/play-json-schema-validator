package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json._
import shapeless.{HNil, Nat}

class ReferenceSpec extends Specification {

  sequential

//  "Root pointer ref" should {
//
//    val schema: QBClass = obj(
//      "foo" -> $ref
//    )
//
//    "match" in {
//      val data = Json.obj(
//        "foo" -> false
//      )
//      Validator.validate(schema)(data).isSuccess must beTrue
//    }
//
//    "recursive match" in {
//      val data = Json.obj(
//        "foo" -> Json.obj(
//          "foo" -> false
//        )
//      )
//      Validator.validate(schema)(data).isSuccess must beTrue
//    }
//
//    "mismatch" in {
//      val data = Json.obj(
//        "bar" -> false
//      )
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//
//    "recursive mismatch" in {
//      val data = Json.obj(
//        "foo" -> Json.obj(
//          "bar" -> false
//        )
//      )
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//  }
//
//  "Relative pointer ref to object" should {
//    val schema = obj(
//      "foo" -> optional(qbInteger),
//      "bar" -> $ref("foo")
//    )
//
//    "match" in {
//      val data = Json.obj(
//        "bar" -> 3
//      )
//      val validationResult = Validator.validate(schema)(data)
//      println(validationResult)
//      validationResult.isSuccess must beTrue
//    }
//
//    "mismatch" in {
//      val data = Json.obj(
//        "bar" -> true
//      )
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//  }
//
//  "Relative pointer ref to array" in {
//    val schema = tuple(
//        qbInteger,
//        $ref("0") // TODO: int parameter
//    )
//
//    "match array" in {
//      val data = Json.arr(1, 2)
//      println(schema)
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isSuccess must beTrue
//    }
//
//    "mismatch array" in {
//      val data = Json.arr(1, "foo")
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//  }

  "Escaped pointer ref" in {
    val properties = "properties"
    val schema = obj(
      "tilda~field" -> optional(qbInteger),
      "slash/field" -> optional(qbInteger),
      "percent%field" -> optional(qbInteger),
//      properties -> obj(
        "tilda" -> $ref("#/tilda~0field"),
        "slash" -> $ref("#/slash~1field"),
        "percent" -> $ref("#/percent%25field")
//      )
    )

    "slash" in {
      val data = Json.obj("slash" -> "aeou")
      val result = Validator.validate(schema)(data)
      println(result)
      result.isFailure must beTrue
    }
  }
}
