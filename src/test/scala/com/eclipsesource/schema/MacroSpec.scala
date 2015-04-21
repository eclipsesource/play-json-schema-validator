package com.eclipsesource.schema

import com.eclipsesource.schema._
import com.eclipsesource.schema.SchemaMacro._
import org.specs2.mutable.Specification

class MacroSpec extends Specification {

  "qb Macro" should {

    "be derivable from an integer attribute"  in {
      case class Foo(number: Int)
      case class Bar(number: Integer)
      deriveSchema[Foo] must beEqualTo(qbClass("number" -> qbInteger))
      deriveSchema[Bar] must beEqualTo(qbClass("number" -> qbInteger))
    }

    "be derivable from a number" in {
      case class Foo(number: Double)
      case class Bar(number: BigDecimal)
      deriveSchema[Foo] must beEqualTo(qbClass("number" -> qbNumber))
      deriveSchema[Bar] must beEqualTo(qbClass("number" -> qbNumber))
    }

    "be derivable from a string" in {
      case class Foo(str: String)
      deriveSchema[Foo] must beEqualTo(qbClass("str" -> qbString))
    }

    "be derivable from a boolean" in {
      val b = qbBoolean
      case class Foo(b: Boolean)
      println(deriveSchema[Foo].prettyPrint)
      deriveSchema[Foo] must beEqualTo(qbClass("b" -> qbBoolean))
    }

    "be derivable from a array" in {
      case class Foo(arr: List[Int])
      deriveSchema[Foo] must beEqualTo(qbClass("arr" -> qbList(qbInteger)))
    }

    "be derivable from a nested case class" in {
      case class Bar(lastName: String)
      case class Foo(name: String, age: Int, bar: Bar)
      val expectedSchema = qbClass(
        "name" -> qbString,
        "age"  -> qbInteger,
        "bar" -> qbClass(
          "lastName" -> qbString
        )
      )
      deriveSchema[Foo] must beEqualTo(expectedSchema)
    }

  }

}
