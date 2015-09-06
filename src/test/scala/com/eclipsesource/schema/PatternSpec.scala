package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import java.net.URL

class PatternSpec extends Specification {

  "Pattern" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/pattern.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

}
