package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import java.net.URL

class MinLengthSpec extends Specification {

  "MinLength" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/minLength.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }

}

