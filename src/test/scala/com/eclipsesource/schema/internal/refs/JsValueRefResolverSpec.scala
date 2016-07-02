package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.refs.JsValueRefResolver._
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

  }
}
