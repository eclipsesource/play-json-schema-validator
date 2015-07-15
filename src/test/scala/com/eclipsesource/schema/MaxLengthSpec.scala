package com.eclipsesource.schema

import java.net.URL
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MaxLengthSpec extends Specification {

  "MaxLength" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/maxLength.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

}
