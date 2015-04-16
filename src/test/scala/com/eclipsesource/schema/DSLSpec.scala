package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._

import scala.math.BigDecimal.long2bigDecimal

object DSLSpec extends Specification {

  "DSL" should {

    val schema = qbClass("time" -> qbPosixTime)

    "have a posix time type" in {
      val currentTime = System.currentTimeMillis() / 1000L
      val instance = Json.obj("time" -> currentTime)
      Validator.validate(schema)(instance).get \ "time" must beEqualTo(JsNumber(currentTime))
    }

    "not validate posix time instances with a double value set" in {
      val instance = Json.obj("time" -> 11.11)
      Validator.validate(schema)(instance).isFailure must beTrue
    }

    "not validate posix time instances with a negative value set" in {
      val instance = Json.obj("time" -> -1000)
      Validator.validate(schema)(instance).isFailure must beTrue
    }
  }
}