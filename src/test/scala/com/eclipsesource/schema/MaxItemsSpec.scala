package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class MaxItemsSpec extends Specification with JsonSpec {
  validate("maxItems")
}

