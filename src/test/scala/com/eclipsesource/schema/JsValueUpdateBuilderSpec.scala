package com.eclipsesource.schema

import java.util.Date

import com.eclipsesource.schema.internal.JsValueUpdateBuilder
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.specs2.mutable.Specification
import play.api.libs.json._

class JsValueUpdateBuilderSpec extends Specification {

  "Mapping over types" should {

    val schema = qbClass(
      "o" -> qbString,
      "x" -> qbList(qbClass(
        "d" -> qbInteger,
        "e" -> qbInteger)))

    val instance = Json.obj(
      "o" -> "foo",
      "x" -> List(Json.obj(
        "d" -> 4,
        "e" -> 5)))

//    "should find all integers" in {
//      val schema = qbClass("s" -> qbString, "i" -> qbInteger)
//      val instance = Json.obj("s" -> "foo", "i" -> 3)
//      val matchedPaths = JsValueUpdateBuilder(schema).byType[QBInteger](identity).matchedPaths(instance)
//      matchedPaths.size must beEqualTo(1)
//    }
//
//    "find all, also nested, integers" in {
//      val schema = qbClass("o" -> qbString, "i" -> qbInteger, "x" -> qbClass("e" -> qbInteger))
//      val instance = Json.obj("o" -> "foo", "i" -> 3, "x" -> Json.obj("e" -> 4))
//      val matchedPaths = JsValueUpdateBuilder(schema).byType[QBInteger](identity).matchedPaths(instance)
//      matchedPaths.size must beEqualTo(2)
//      matchedPaths(1)._2 must beEqualTo(JsPath() \ "x" \ "e")
//    }
//
//    "find integers in an array" in {
//      val schema = qbClass("o" -> qbString, "x" -> qbList(qbClass("d" -> qbInteger, "e" -> qbInteger)))
//      val instance = Json.obj("o" -> "foo", "x" -> List(Json.obj("d" -> 4, "e" -> 5)))
//      val matchedPaths = JsValueUpdateBuilder(schema).byType[QBInteger](identity).matchedPaths(instance)
//      matchedPaths.size must beEqualTo(2)
//    }

//    "find and increment integers in an array" in {
//
//      val schema = qbClass(
//        "o" -> qbString,
//        "x" -> qbList(
//          qbClass("d" -> qbInteger,
//            "e" -> qbInteger)))
//
//      val instance = Json.obj(
//        "o" -> "foo",
//        "x" -> List(Json.obj(
//          "d" -> 4,
//          "e" -> 5)))
//
//      val updatedObject = JsValueUpdateBuilder(schema).byType[QBInteger](inc(1)).go(instance)
//      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsNumber(5))
//      (updatedObject \ "x")(0) \ "e" must beEqualTo(JsNumber(6))
//    }

    "find and increment integers in an array via map" in {
      val updatedObject = schema.transform(instance)(
        isQBInteger -> { case JsNumber(n) => JsNumber(n + 1) }
      )

      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsNumber(5))
      (updatedObject \ "x")(0) \ "e" must beEqualTo(JsNumber(6))
    }

    "find and increment integers in an array via builder" in {
      val now = new Date().toString
      val schema = qbClass(
        "o" -> qbString,
        "x" -> qbList(qbClass(
          "d" -> qbDateTime,
          "e" -> qbInteger)))

      val instance = Json.obj(
        "o" -> "foo",
        "x" -> List(Json.obj(
          "d" -> Json.obj("$date" -> now),
          "e" -> 5)))

      val updatedSchema = schema
        .updateByType[QBDateTime](attr => qbClass("$date" -> qbDateTime))

      val builder = new JsValueUpdateBuilder(updatedSchema).byTypeAndPredicate[QBClass](_.hasAttribute("$date")) {
        case o: JsObject => o.fieldSet.find(_._1 == "$date").get._2
      }

      val updatedObject = builder.go(instance)

      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsString(now))
    }

    "find and increment integers in an array via builder directly" in {

      val now = new Date().toString

      val schema = qbClass(
        "x" -> qbList(qbDateTime))

      val instance = Json.obj(
        "x" -> List(Json.obj("$date" -> now)))

      val updatedSchema = schema
        .updateByType[QBDateTime](qbType => qbClass("$date" -> qbDateTime))

      val builder = new JsValueUpdateBuilder(updatedSchema).byType[QBClass] {
        case o: JsObject if o.fieldSet.exists(_._1 == "$date") => o.fieldSet.find(_._1 == "$date").get._2
        case o => o
      }

      val updatedObject = builder.go(instance)

      (updatedObject \ "x")(0) must beEqualTo(JsString(now))
    }

//    "find and uppercase all strings via toUpperCase" in {
//      val updatedObject = schema.transform(instance) {
//        isQBString -> toUpperCase
//      }
//      (updatedObject \ "o") must beEqualTo(JsString("FOO"))
//    }
//
//    "find and convert numbers to strings" in {
//      val updatedObject = schema.transform(instance)(
//        isQBInteger -> { case JsNumber(n) => JsString(n.intValue().toString) },
//        isQBString -> toUpperCase
//      )
//      (updatedObject \ "o") must beEqualTo(JsString("FOO"))
//      (updatedObject \ "x")(0) \ "d" must beAnInstanceOf[JsString]
//      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsString("4"))
//    }

    "find and uppercase all strings and increment all ints via mapping builder" in {
      val mappingBuilder = new JsValueUpdateBuilder(schema).byType[QBString] {
        case JsString(s) => JsString(s.toUpperCase)
      }.byType[QBInteger] {
        case JsNumber(n) => JsNumber(n + 1)
      }
      val updatedObject = mappingBuilder.go(instance)
      (updatedObject \ "o") must beEqualTo(JsString("FOO"))
      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsNumber(5))
      (updatedObject \ "x")(0) \ "e" must beEqualTo(JsNumber(6))
    }
    
    "convert datetime and posixtime dates to string" in {
      val date = new DateTime(2000,1,1,1,1)
      val expected = "01.01.2000"

      val dateString = date.toString()
      val dateMillis = date.getMillis

      val sampleSchema = qbClass(
        "d" -> qbDateTime,
        "e" -> qbPosixTime)

      val sampleJson = Json.obj(
        "d" -> dateString, 
        "e" -> dateMillis)

      val expectedJson = Json.obj(
        "d" -> expected, 
        "e" -> expected)

      def formatDate(date: DateTime) = DateTimeFormat.forPattern("dd.MM.yyyy").print(date)
      val transformer = new JsValueUpdateBuilder(sampleSchema)
        .byType[QBDateTime] {
          case JsString(dateTime) => JsString(formatDate(DateTime.parse(dateTime)))
          case j => j
        }.byType[QBPosixTime] {
          case JsNumber(time) => JsString(formatDate(new DateTime(time.toLong)))
          case j => j
        }

      transformer.go(sampleJson) must beEqualTo(expectedJson)
    } 

    "distinguish posixdates and numbers" in {
      val date = new DateTime(2000,1,1,1,1)
      val dateMillis = date.getMillis

      val sampleSchema = qbClass(
        "e" -> qbPosixTime,
        "n" -> qbNumber)

      val sampleJson = Json.obj(
        "e" -> dateMillis,
        "n" -> 1)

      val expectedJson = Json.obj(
        "e" -> "01.01.2000", 
        "n" -> 2)

      def formatDate(date: DateTime) = DateTimeFormat.forPattern("dd.MM.yyyy").print(date)
      val transformer = new JsValueUpdateBuilder(sampleSchema)
        .byType[QBPosixTime] {
          case JsNumber(time) => JsString(formatDate(new DateTime(time.toLong)))
          case j => j
        }.byType[QBNumber] {
          case JsNumber(num) => JsNumber(num + 1)
          case j => j
        }

      transformer.go(sampleJson) must beEqualTo(expectedJson)
    } 
  }
}