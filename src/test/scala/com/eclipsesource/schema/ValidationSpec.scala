package com.eclipsesource.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.mapping.Success
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._

import scala.math.BigDecimal.int2bigDecimal

@RunWith(classOf[JUnitRunner])
object ValidationSpec extends Specification {

  "Validator" should {

    "accept valid JSON instance" in {
      val schema = qbClass(
        "o" -> qbClass(
          "s" -> qbString(minLength(5))
        )
      )
      val instance = Json.obj(
        "o" -> Json.obj(
          "s" -> "hallo")
      )

      Validator.validate(schema)(instance).asOpt must beSome(instance)
    }

    "not accept invalid JSON instance" in {
      val schema = qbClass(
        "o" -> qbClass(
          "s" -> qbString(minLength(5))))
      val instance = Json.obj(
        "o" -> Json.obj(
          "s" -> "hell"))

      Validator.validate(schema)(instance).asOpt must beNone
    }

    "allow default values" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> qbNumber,
        "d" -> default(qbNumber, JsNumber(42)))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      Validator.validate(schema)(instance) must beEqualTo (Success(Json.obj(
        "o" -> "o",
        "e" -> 23,
        "d" -> 42
      )))
    }

    "validate objects with missing fields if they are marked as optional" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> qbNumber,
        "d" -> optional(qbNumber))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      Validator.validate(schema)(instance) must beEqualTo (Success(instance))
    }

    "not validate objects with missing fields successfully if optional is missing" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> qbNumber,
        "d" -> qbNumber)
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      Validator.validate(schema)(instance).asOpt must beNone
    }

    "validate nested optional successfully" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> qbNumber,
        "d" -> optional(qbClass("i" -> optional(qbString))))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "validate nested optional successfully where inner optional is missing" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> qbNumber,
        "d" -> optional(qbClass(
          "x" -> optional(qbString)
        )))

      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23,
        "d" -> Json.obj())

      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }


    "validate optional with provided default value and already set instance value successfully" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> qbNumber,
        "d" -> optional(qbNumber, JsNumber(11)))

      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23,
        "d" -> 14)

      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "validate optional even if value is null" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> optional(qbNumber)
      )

      val instance = Json.obj(
        "o" -> "foo",
        "e" -> JsNull
      )

      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "validate optional if value is undefined" in {
      val schema = qbClass(
        "o" -> qbString,
        "e" -> optional(qbNumber)
      )

      val instance = Json.obj(
        "o" -> "foo",
        "e" -> JsUndefined("")
      )

      Validator.validate(schema)(instance) must beEqualTo(Success(Json.obj("o" -> "foo")))
    }

    "validate multipleOf if integer is a valid multiple of" in {
      val schema = qbClass(
        "i" -> qbInteger(multipleOf(3)))
      val instance = Json.obj("i" -> 9)
      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }

    "not validate multipleOf if integer is not a invalid multiple of" in {
      val schema = qbClass(
        "i" -> qbInteger(multipleOf(3))
      )
      val instance = Json.obj(
        "i" -> 10
      )
      Validator.validate(schema)(instance).asOpt must beNone
    }


    "validate recursive schemas definitions" in {
      // type annotation is mandatory
      lazy val schema: QBClass = qbClass(
        "author" -> qbString,
        "message" -> qbString,
        "replies" -> qbList(schema)
      )
      val instance = Json.obj(
        "author" -> "Dude",
        "message" -> "Hallo?",
        "replies" -> Json.arr(
          Json.obj(
            "author" -> "Dudezzzz",
            "message" -> "Hi!",
            "replies" -> Json.arr()
          )
        )
      )

      Validator.validate(schema)(instance) must beEqualTo(Success(instance))
    }


//
//    "test tolerant numbers conversion" in {
//      val schema = qbClass(List(
//        "i" -> qbInteger,
//        "j" -> qbNumber))
//      val instance = Json.obj(
//        "i" -> 9,
//        "j" -> "10")
//      Validator.validate(schema)(instance).asOpt must beSome
//    }
//
//    "test tolerant integer conversion" in {
//      val schema = qbClass(List(
//        "i" -> qbInteger,
//        "j" -> qbInteger))
//      val instance = Json.obj(
//        "i" -> 9,
//        "j" -> "10")
//      Validator.validate(schema)(instance).asOpt must beSome
//    }
//
//    "test error case when tolerant number conversion gets non valid number" in {
//      val schema = qbClass(List(
//        "i" -> qbInteger,
//        "j" -> qbNumber))
//      val instance = Json.obj(
//        "i" -> 9,
//        "j" -> "10aaaa")
//      Validator.validate(schema)(instance).asOpt must beNone
//    }
//
//    "test tolerant boolean conversion false" in {
//      val schema = qbClass(List(
//        "i" -> qbInteger,
//        "j" -> qbBoolean))
//      val instance = Json.obj(
//        "i" -> 9,
//        "j" -> "fALSE")
//      val result = Validator.validate(schema)(instance)
//      result.asOpt must beSome
//    }
//
//    "test tolerant boolean conversion with true" in {
//      val schema = qbClass(List(
//        "i" -> qbInteger,
//        "j" -> qbBoolean))
//      val instance = Json.obj(
//        "i" -> 9,
//        "j" -> "true")
//      Validator.validate(schema)(instance).asOpt must beSome
//    }
//
//    "test error case when tolerant boolean conversion gets non valid boolean" in {
//      val schema = qbClass(List(
//        "i" -> qbInteger,
//        "j" -> qbBoolean))
//      val instance = Json.obj(
//        "i" -> 9,
//        "j" -> "MaybeTrue")
//      Validator.validate(schema)(instance).asOpt must beNone
//    }
//
//    "test case class conversion" in {
//      import play.api.libs.json._
//
//      case class Person(name: String, age: Int)
//
//      object Person{
//        implicit val implicitPersonWrites = new Writes[Person] {
//          def writes(person: Person): JsValue = {
//            Json.obj(
//              "name" -> person.name,
//              "age" -> person.age
//            )
//          }
//        }
//      }
//
//      val person = Person("", 46)
//      val personSchema = qbClass(
//        "name" -> qbNonEmptyText,
//        "age" -> qbInteger
//      )
//      val personJs: JsValue = Json.toJson(person)
//      Validator.validate(personSchema)(personJs) must beAnInstanceOf[JsSuccess[JsObject]]
//    }
  }

}