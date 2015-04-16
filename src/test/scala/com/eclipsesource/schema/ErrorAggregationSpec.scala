package com.eclipsesource.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.mapping.{ValidationError, Path, Failure}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._

@RunWith(classOf[JUnitRunner])
class ErrorAggregationSpec extends Specification {

  "Validator" should {

    "emit errors for all fields if empty input object" in {

      val schema = qbClass(
        "ratingStatus" -> qbEnum("pending", "approved", "rejected"),
        "rating" -> qbInteger,
        "comment" -> qbString(maxLength(2000)),
        "firstName" -> optional(qbString(com.eclipsesource.schema.length(1, 50))),
        "lastName" -> qbString(com.eclipsesource.schema.length(1, 50)),
        "ratingDate" -> qbDateTime)

      val result = Validator.validate(schema)(Json.obj()).asInstanceOf[Failure[(Path, Seq[ValidationError]), JsValue]]
      result.errors.length must beEqualTo(5)
      result.errors(0)._1 must beEqualTo(Path \ "ratingStatus")
      result.errors(1)._1 must beEqualTo(Path \ "rating")
      result.errors(2)._1 must beEqualTo(Path \ "comment")
      // firstname is optional
      result.errors(3)._1 must beEqualTo(Path \ "lastName")
      result.errors(4)._1 must beEqualTo(Path \ "ratingDate")
    }

    "emit errors for optional value with wrong type" in {
      val schema = qbClass("name" -> optional(qbString))
      val data = Json.obj("name" -> 1234)

      val result = Validator.validate(schema)(data).asInstanceOf[Failure[(Path, Seq[ValidationError]), JsValue]]
      result.errors(0)._1 must beEqualTo(Path \ "name")
    }

    "emit errors for a nested object that has an property with a wrong type" in {
      val schema = qbClass(
        "name" -> qbClass(
          "firstName" -> qbString,
          "lastName" -> qbString)
      )
      val data = Json.obj("name" ->
        Json.obj(
          "firstName" -> "Otto",
          "lastName" -> 23)
      )

      val result = Validator.validate(schema)(data).asInstanceOf[Failure[(Path, Seq[ValidationError]), JsValue]]

      result.errors.size must beEqualTo(1)
      result.errors(0)._1 must beEqualTo(Path \ "name" \ "lastName")
    }

    "emit errors for an array with various instance types" in {

      val schema = qbClass(
        "arr" -> qbList(qbString)
      )

      val data = Json.obj(
        "arr" -> Json.arr(true, 2)
      )

      val result = Validator.validate(schema)(data).asInstanceOf[Failure[(Path, Seq[ValidationError]), JsValue]]

      result.errors.size must beEqualTo(2)
      result.errors(0)._2(0).message must contain("qb.diff.types")
    }

    "emit error in case the instance violates the array items type" in {
      val schema = qbClass(
        "obj" -> qbClass(
          "arr" -> qbList(qbString)
        )
      )
      val instance = Json.obj(
        "obj" -> Json.obj(
          "arr" -> List(5)
        )
      )

      val result = Validator.validate(schema)(instance).asInstanceOf[Failure[(Path, Seq[ValidationError]), JsValue]]
      result.errors(0)._2(0).message must beEqualTo("qb.diff.types")
      result.asOpt.isDefined must beFalse
    }

    "emit error message in case the instance does not adhere the schema" in {
      val schema = qbClass(
        "o" -> qbClass(
          "s" -> qbString(minLength(5))))
      val instance = Json.obj(
        "o" -> Json.obj(
          "s" -> 5))
      val result = Validator.validate(schema)(instance)
      result.asOpt.isDefined must beFalse
    }

    "return errors for an object with duplicate field names" in {
      qbClass(
        "a" -> qbList(qbInteger),
        "a" -> qbNumber) must throwA[RuntimeException]("qb.duplicate.fields - a")
    }
  }
}