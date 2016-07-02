package com.eclipsesource.schema.internal.refs

import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsString, Json}

class JsValueRefResolverSpec extends Specification {

  "JsValueRefResolver" should {

    val obj = Json.obj(
      "foo" -> Json.arr("bar", "baz"),
      ""   -> 0,
      "a/b" -> 1
    )

    "resolve" in {
      val bar = JsValueRefResolver.resolve("#/foo/0", obj)
      val arr = JsValueRefResolver.resolve("#/foo", obj)
      val number = JsValueRefResolver.resolve("#/", obj)
      val ab = JsValueRefResolver.resolve("#/a~1b", obj)

      bar must beRight(JsString("bar"))
      arr must beRight(Json.arr(JsString("bar"), JsString("baz")))
      number must beRight(JsNumber(0))
      ab must beRight(JsNumber(1))
    }

    "must not resolve with invalid scope" in {
      JsValueRefResolver.resolve("#/foo/0", JsNumber(0)) must beLeft
    }

//    "must resolve relative refs" in {
//      val sample = Json.obj(
//        "foo" -> Json.arr("bar", "baz"),
//        "highly" -> Json.obj(
//          "nested" -> Json.obj(
//            "objects" -> "true"
//          )
//        )
//      )
//
//      JsValueRefResolver.resolveRelative("0", "#/foo/1", sample) must beRight(JsString("baz"))
//    }
  }
}
