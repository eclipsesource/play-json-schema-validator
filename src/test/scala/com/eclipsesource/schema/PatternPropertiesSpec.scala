package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class PatternPropertiesSpec extends Specification {

  "PatternProperties" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/patternProperties.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(_.execute)
    }
  }
}
