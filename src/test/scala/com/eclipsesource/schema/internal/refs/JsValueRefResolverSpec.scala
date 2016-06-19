package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.refs.JsonRefResolver._
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
      val scope = new JsValueResolutionScope(obj)
      val resolver = new JsValueRefResolver
      val bar = resolver.resolve("#/foo/0", scope)
      val arr = resolver.resolve("#/foo", scope)
      val number = resolver.resolve("#/", scope)
      val ab = resolver.resolve("#/a~1b", scope)

      bar must beRight(JsString("bar"))
      arr must beRight(Json.arr(JsString("bar"), JsString("baz")))
      number must beRight(JsNumber(0))
      ab must beRight(JsNumber(1))
    }
  }
}
