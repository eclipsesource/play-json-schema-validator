package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class DependenciesSpec extends Specification {

  "Dependencies" should {

    val resourceUrl: URL = getClass.getResource("/draft4/dependencies.json")

    "validate" in {
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }
}
