package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNull, Json}

class AdditionalItemsSpec extends Specification {

  "AdditionalItems" should {

    val resourceUrl: URL = getClass.getResource("/draft4/additionalItems.json")

    "validate" in {
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(_.execute)
    }
  }
}
