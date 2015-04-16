package com.eclipsesource.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.{JsNumber, JsString, Json}

import scala.math.BigDecimal.int2bigDecimal

@RunWith(classOf[JUnitRunner])
object JSONSchemaExtensionWritesSpec extends Specification {

  "JSONSchema extensions" should {

    "to JSON schema with min rule" in {
      val qbSchema = qbClass(
        "n" -> default(qbNumber, JsNumber(3)),
        "o" -> readOnly(qbNumber))
      val jsonSchema = Json.toJson(qbSchema)(qbTypeWriter)
      (jsonSchema \ "properties" \ "n" \ "default") must beEqualTo(JsNumber(3))
      (jsonSchema \ "properties" \ "o" \ "readonly") must beEqualTo(JsString("true"))
    }

    /**
     * "type": "object",
     * "properties": {
     * "firstName": {
     * "type": "string"
     * },
     * "lastName": {
     * "type": "string"
     * },
     * "age": {
     * "description": "Age in years",
     * "type": "integer",
     * "minimum": 0
     * }
     * },
     * "required": ["firstName", "lastName"]
     */
    "to JSON schema with min and max rule" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> optional(qbInteger(min(0))))
      val jsonSchema = Json.toJson(qbSchema)
      true must beTrue
    }

    "to JSON schema with min and max rule" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> optional(qbInteger(min(0), max(10))))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "minimum").toString must contain("0")
    }

    "to JSON schema with range rule" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> optional(qbInteger(range(0, 10))))
      val jsonSchema = Json.toJson(qbSchema)
      (jsonSchema \ "age" \ "minimum").toString must contain("0")
      (jsonSchema \ "age" \ "maximum").toString must contain("10")
    }

    "to JSON schema with number range rule" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> optional(qbNumber(range(0.5, 11.43))))
      val jsonSchema = Json.toJson(qbSchema)
      (jsonSchema \ "age" \ "minimum").toString must contain("0.5")
      (jsonSchema \ "age" \ "maximum").toString must contain("11.43")
    }

    "work with object if required isn't set" in {
      val schemaAsJson = Json.obj(
        "type" -> "object",
        "properties" -> Json.obj(
          "hallo" -> Json.obj(
            "type" -> "string")))
      val parsedSchema = Json.fromJson(schemaAsJson)(valueReader)

      parsedSchema.asOpt.isDefined must beTrue
      parsedSchema.asOpt.get must beEqualTo(qbClass(
        "hallo" -> optional(qbString)))
    }
  }

}