package com.eclipsesource.schema

import java.net.URL
import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.RefResolver
import com.eclipsesource.schema.test.{JsonSpec, JSONSource}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsString, JsNumber, JsValue, Json}

class ReferenceSpec extends Specification {

  "References" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/ref.json")

      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

//  "Root pointer ref" should {
//
//    val schema = JSONSource.schemaFromString(
//      """{
//        |"properties": {
//        |  "foo": {"$ref": "#"}
//        |},
//        |"additionalProperties": false
//      }""".stripMargin).get
//
//    println(Json.prettyPrint(Json.toJson(schema)))
//
//
//    "match" in {
//      val data = Json.obj(
//        "foo" -> false
//      )
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isSuccess must beTrue
//    }
//  }
    //
//    "recursive match" in {
//      val data = Json.obj(
//        "foo" -> Json.obj(
//          "foo" -> false
//        )
//      )
//      val result = Validator.validate(schema)(data)
//      result.isSuccess must beTrue
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
//    val data = Json.obj(
//      "foo" -> Json.obj(
//        "bar" -> false
//      )
//    )
//    Validator.validate(schema)(data).isFailure must beTrue
//  }
//  }
//
//  "Relative pointer ref to object" should {
//
//    val schema = JSONSource.schemaFromString(
//      """{
//        |"properties": {
//        |  "foo": {"type": "integer"},
//        |  "bar": {"$ref": "#/properties/foo"}
//        |}
//      }""".stripMargin).get
//
//    println(Json.prettyPrint(Json.toJson(schema)))
//
//    "match" in {
//      val data = Json.obj(
//        "bar" -> 3
//      )
//      val validationResult = Validator.validate(schema)(data)
//      println(validationResult)
//      validationResult.isSuccess must beTrue
//    }
//  }
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
//    val schema = obj(
//      "items" -> tuple2(
//        qbInteger,
//        obj("$ref" -> $ref("#/items/0")) // TODO: int parameter
//      )
//    )
//
//
//    "match array" in {
//      val data = Json.arr(1, 2)
//      val result = Validator.validate(schema)(data)
//      result.isSuccess must beTrue
//    }
//
//    "mismatch array" in {
//      val data = Json.arr(1, "foo")
//      Validator.validate(schema)(data).isFailure must beTrue
//    }
//  }
//
//  "Escaped pointer ref" should {
//    val schema = obj(
//      "tilda~field" -> optional(qbInteger),
//      "slash/field" -> optional(qbInteger),
//      "percent%field" -> optional(qbInteger),
//      "properties" -> obj(
//        "tilda" -> optional(
//          $ref("#/tilda~0field"))
//        ,
//        "slash" -> optional(
//          $ref("#/slash~1field")
//        ),
//        "percent" -> optional(
//          $ref("#/percent%25field")
//        )
//      )
//    )
//
//    "slash" in {
//      val data = Json.obj("slash" -> "aeou")
//      val result = Validator.validate(schema)(data)
//      result.isFailure must beTrue
//    }
//
//    "tilda" in {
//      val data = Json.obj("tilda" -> "aeou")
//      val result = Validator.validate(schema)(data)
//      result.isFailure must beTrue
//    }
//
//    "percent" in {
//      val data = Json.obj("percent" -> "aeou")
//      val result = Validator.validate(schema)(data)
//      result.isFailure must beTrue
//    }
//  }
//
  "nested refs" should {

    val schema = JSONSource.schemaFromString(
      """{
        |  "definitions": {
        |    "a": {"type": "integer"},
        |    "b": {"$ref": "#/definitions/a"},
        |    "c": {"$ref": "#/definitions/b"}
        |  },
        |  "$ref": "#/definitions/c"
        |}""".stripMargin).get
//
//
//    "nested ref valid" in {
//      val data: JsValue = JsNumber(5)
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isSuccess must beTrue
//    }
//
    "nested ref invalid" in {
      val data: JsValue = JsString("a")
      val result = Validator.validate(schema)(data)
      result.isFailure must beTrue
    }
//
  }
//
//  "remote ref, containing refs itself" should {
////    val schema = $ref("http://json-schema.org/draft-04/schema#")
//
//    sequential
//
//    val schema = JSONSource.schemaFromString(
//      """{
//        |"$ref": "http://json-schema.org/draft-04/schema#"
//      }""".stripMargin).get
//
//    "remote ref valid" in {
//      val data = Json.obj("minLength" -> 1)
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isSuccess must beTrue
//    }
//
//    "remote ref invalid" in {
//      val data = Json.obj("minLength" -> -1)
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isFailure must beTrue
//    }
//  }
}
