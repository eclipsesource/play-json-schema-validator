package com.eclipsesource.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.Json

import com.eclipsesource.schema._

@RunWith(classOf[JUnitRunner])
object JSONSchemaWritesSpec extends Specification {

  "Schema writes" should {

    "to JSON schema with min rule" in {
      val qbSchema = qbClass(
        "n" -> qbNumber(min(10)))
      val jsonSchema = Json.toJson(qbSchema)
      (jsonSchema \ "n" \ "minimum").toString must contain("10")
    }

    "to JSON schema with max rule" in {
      val qbSchema = qbClass(
        "n" -> qbNumber(max(10)))
      val jsonSchema = Json.toJson(qbSchema)
      //      val j = Json.fromJson(jsonSchema)
      //      val schema: QBValue = j.get
      (jsonSchema \ "n" \ "maximum").toString must contain("10")
    }

    "to JSON schema with min and max rule" in {
      val qbSchema = qbClass(
        "n" -> qbNumber(min(5), max(10)))
      val jsonSchema = Json.toJson(qbSchema)
      (jsonSchema \ "n" \ "minimum").toString must contain("5")
      (jsonSchema \ "n" \ "maximum").toString must contain("10")
    }

    "to JSON schema with integer only" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbInteger)
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      true must beTrue
    }

    "to JSON schema with number only" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber)
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      true must beTrue
    }

    "to JSON schema with number with min" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber(min(10.0)))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "minimum").toString must contain("10")
    }

    "to JSON schema with number with max" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber(max(20)))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "maximum").toString must contain("20")
    }

    "to JSON schema with number with min and isExclusive" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber(exclusiveMin(10.0)))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "minimum").toString must contain("10")
      (jsonSchema \ "age" \ "exclusiveMinimum ").toString must contain("true")
    }

    "serialize references" in {
      val schema = definitions(Map(
        "address" -> qbClass(
          "street_address" -> qbString
        )
      ))
      { defs => qbClass(
        "billing_address" -> defs("address"),
        "shipping_address" -> defs("address")
      )
      }


      val jsonSchema = Json.toJson(schema)
      println(Json.prettyPrint(jsonSchema))
      //      jsonSchema \ "billing_address" \ "address" \ ""
      true must beTrue
    }
  }

}