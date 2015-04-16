package com.eclipsesource.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.mapping.Success
import play.api.libs.json._

@RunWith(classOf[JUnitRunner])
class RulesSpec extends Specification {

  "String Rules" should {

    "validate non-empty strings" in {
      qbNonEmptyText.validate(JsString("a")).isSuccess must beTrue
      qbNonEmptyText.validate(JsString("")).isFailure must beTrue
    }

    "validate minimal length" in {
      val text = "0123456789"
      minLength(4).validate(JsString(text)).isSuccess must beTrue
      minLength(11).validate(JsString(text)).isFailure must beTrue
    }

    "validate maximum length" in {
      val text = "0123456789"
      maxLength(11).validate(JsString(text)).isSuccess must beTrue
      maxLength(4).validate(JsString(text)).isSuccess must beFalse
    }

    "validate enum strings" in {
      qbEnum("eddy", "otto", "dude").validate(JsString("dude")).isSuccess must beTrue
      qbEnum("eddy", "otto", "dude").validate(JsString("honk")).isFailure must beTrue
    }

    "validate pattern (email)" in {
      qbEmail.validate(JsString("otto@m-cube.de")).isSuccess must beTrue
      qbEmail.validate(JsString("dude@@dude")).isFailure must beTrue
    }

  }

  "Number Rules" should {

    "validate min constraint" in {
      min(10).validate(JsNumber(10)).isSuccess must beTrue
      min(10).validate(JsNumber(5)).isFailure must beTrue
    }

    "validate max constraint" in {
      max(10).validate(JsNumber(5)).isSuccess must beTrue
      max(10).validate(JsNumber(11)).isFailure must beTrue
    }

  }

  "Boolean Rules" should {

    "validate booleans" in {
      qbBoolean.validate(JsBoolean(value = true)).isSuccess must beTrue
      qbBoolean.validate(JsBoolean(value = false)).isSuccess must beTrue
    }

  }

  "Array Rules" should {

    "validate unique constraint successfully if the list only contains distinct elements" in {
      qbList(qbNumber, unique).validate(Json.arr(1, 2, 3)).isSuccess must beTrue
    }

    "not validate unique constraitn  if a list contains duplicates" in {
      qbList(qbNumber, unique).validate(Json.arr(1, 2, 3, 3)).isSuccess must beFalse
    }

    "validate minItems constraint successfully if the list contains at least the specified number of elements" in {
      qbList(qbNumber, minItems(3)).validate(Json.arr(1, 2, 3)).isSuccess must beTrue
    }

    "not validate minItems constraint if the list contains less than the specified number of elements" in {
      qbList(qbNumber, minItems(3)).validate(Json.arr(1, 2)).isSuccess must beFalse
    }

    "validate maxItems constraint successfully if the list contains at most the specified number of elements" in {
      qbList(qbNumber, maxItems(3)).validate(Json.arr(1, 2, 3)).isSuccess must beTrue
    }

    "not validate maxItems constraint if the list contains more than the specified number of elements" in {
      qbList(qbNumber, maxItems(3)).validate(Json.arr(1, 2, 3, 4)).isSuccess must beFalse
    }
  }

  "Object rules" should {

    // oneOf constraint --

    "validate objects with oneOf constraint successfully" in {

      val schema = qbClass(
        "i" -> com.eclipsesource.schema.oneOf(
          qbClass("x" -> qbNumber),
          qbClass("x" -> qbString))
      )

      val instanceWithString = Json.obj(
        "i" -> Json.obj(
          "x" -> "wat"
        )
      )

      val instanceWithNumber = Json.obj(
        "i" -> Json.obj(
          "x" -> "wat"
        )
      )

      Validator.validate(schema)(instanceWithString) must beEqualTo(Success(instanceWithString))
      Validator.validate(schema)(instanceWithNumber) must beEqualTo(Success(instanceWithNumber))
    }

    "not validate object with oneOf constraint if instance does not match schema" in {
      val schema = qbClass(
        "i" -> com.eclipsesource.schema.oneOf(
          qbClass("x" -> qbNumber),
          qbClass("x" -> qbString))
      )
      Validator.validate(schema)(Json.obj("j" -> Json.obj("xx" -> "wat"))).asOpt must beNone
    }

    "not validate object with oneOf constraint if instance matches more than one schemas" in {
      val schema = qbClass(
        "i" -> com.eclipsesource.schema.oneOf(
          qbClass("n" -> qbNumber),
          qbClass("s" -> qbString)))
      val instance = Json.obj("j" -> Json.obj("n" -> 3, "s" -> "wat"))
      Validator.validate(schema)(instance).asOpt must beNone
    }

    // allOf constraint --

    "validate allOf object constraint if instance matches all of the given schemas" in {
      val schema = qbClass(
        "i" -> com.eclipsesource.schema.allOf(
          qbClass("n" -> qbNumber),
          qbClass("s" -> qbString)
        )
      )
      val instance = Json.obj(
        "i" -> Json.obj(
          "n" -> 3,
          "s" -> "wat"
        )
      )
      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "not validate allOf object constraint if instance matches only one of the schemas" in {
      val allOfSchema = qbClass(
        "i" -> com.eclipsesource.schema.allOf(
          qbClass("n" -> qbNumber),
          qbClass("s" -> qbString)
        )
      )
      Validator.validate(allOfSchema)(
        Json.obj("i" -> Json.obj(
          "n" -> 3,
          "w" -> "wat")
        )
      ).asOpt must beNone
    }


    // anyOf constraint --

    "not validate anyOf object constraint if name of constrained property is wrong" in {
      val anyOfSchema = qbClass(
        "i" -> com.eclipsesource.schema.anyOf(
          qbClass("n" -> qbNumber),
          qbClass("s" -> qbString)
        )
      )
      // field name 'j' instead of 'i'
      Validator.validate(anyOfSchema)(
        Json.obj(
          "j" -> Json.obj(
            "n" -> 3,
            "s" -> "wat")
        )
      ).asOpt must beNone
    }

    "validate anyOf object constraint if more than one schema matches" in {
      val schema = qbClass(
        "i" -> com.eclipsesource.schema.anyOf(
          qbClass("n" -> qbNumber),
          qbClass("s" -> qbString)
        )
      )
      val instance = Json.obj(
        "i" -> Json.obj(
          "n" -> 3,
          "s" -> "wat"
        )
      )
      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "validate anyOf object constraint if exactly schemas matches" in {
      val schemas = qbClass(
        "i" -> com.eclipsesource.schema.anyOf(
          qbClass("n" -> qbNumber),
          qbClass("s" -> qbString)
        )
      )
      val instance = Json.obj("i" -> Json.obj("n" -> 3))
      Validator.validate(schemas)(instance) must beEqualTo(Success(instance))
    }

    // minProperties --

    "validate with minProperties constraint if instance has at least the given minimum number of properties" in {
      val schema = qbClass(List(
        "i" -> qbInteger,
        "j" -> qbInteger),
        minProperties(1)
      )
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10
      )
      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "not validate with minProperties constraint if instance has less properties than the given minimum " in {
      val schema = qbClass(List(
        "i" -> qbInteger,
        "j" -> qbInteger),
        minProperties(3)
      )
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10
      )
      Validator.validate(schema)(instance).asOpt must beNone
    }

    // maxProperties --

    "validate with maxProperties constraint if instance has less or equal number of properties " in {
      val schema = qbClass(List(
        "i" -> qbInteger,
        "j" -> qbInteger),
        maxProperties(3))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10)
      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "not validate with maxProperties constraint if instance has more properties than allowed" in {
      val schema = qbClass(List(
        "i" -> qbInteger,
        "j" -> qbInteger),
        maxProperties(1))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10)
      Validator.validate(schema)(instance).asOpt must beNone
    }
  }
}