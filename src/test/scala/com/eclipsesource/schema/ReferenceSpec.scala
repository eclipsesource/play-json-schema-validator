package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json._
import shapeless.{HNil, Nat}

class ReferenceSpec extends Specification {

  "References" should {

    object cust
    object address
    object bird

    case class Foo()

    val customer = definitions { Map(
      // TODO: no convenient syntax for constants available
      //      "$schema" -> default(qbString, JsString("http://json-schema.org/draft-04/schema")),
      "address" -> qbClass(
        "street_address" -> qbString,
        "city" -> qbString,
        "state" -> qbString
      )
    )} { defs =>
      qbClass(
        "billing_address" -> defs("address"),
        "shipping_address" -> defs("address")
      )
    }

    "should be inlined if expand is called on a schema" in {
      println(Json.prettyPrint(Json.toJson(customer)))
      val expandedSchema: QBClass = customer.expand
      println(Json.prettyPrint(Json.toJson(expandedSchema)))
      expandedSchema("billing_address") must beAnInstanceOf[QBClass]
      true must beTrue
    }

    ""

    //    "allow reuse of schema" in {
    //      val foo = "foo"
    //      val r: foo.type = foo
    //      println(r)
    //      val attributes: shapeless.::[address.type, shapeless.::[cust.type, HNil]] = address :: cust :: HNil
    //      Nat.toInt(attributes.filter[address.type].length) must beEqualTo(1)
    //      Nat.toInt(attributes.filter[bird.type].length) must beEqualTo(0)
    //    }
  }

}
