package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JsonSpec}
import org.specs2.mutable.Specification

class AdditionalItemsSpec extends Specification {

  "AdditionalItems" should {

    val resourceUrl: URL = getClass.getResource("/draft4/additionalItems.json")

    "validate" in {
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(_.execute)
    }
  }

}
