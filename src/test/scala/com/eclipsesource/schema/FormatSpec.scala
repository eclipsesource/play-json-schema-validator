package com.eclipsesource.schema

import org.specs2.mutable.Specification
import java.net.URL
import com.eclipsesource.schema.test.JsonSpec

class FormatSpec extends Specification {

  "Format" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/optional/format.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

}
