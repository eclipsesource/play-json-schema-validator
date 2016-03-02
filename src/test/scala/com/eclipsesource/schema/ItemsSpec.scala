package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class ItemsSpec extends Specification with JsonSpec {
  validate("items")
}

