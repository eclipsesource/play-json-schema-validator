package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class EnumSpec extends Specification {

  "Enum" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/enum.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

}
