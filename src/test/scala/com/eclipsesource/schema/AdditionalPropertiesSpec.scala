package com.eclipsesource.schema

import java.net.URL
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class AdditionalPropertiesSpec extends Specification {

  "AdditionalProperties" should {

    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/additionalProperties.json")

      "validate" in {
        foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
      }
    }

  }
}
