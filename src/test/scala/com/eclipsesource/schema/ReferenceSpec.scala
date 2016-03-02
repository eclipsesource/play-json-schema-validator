package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class ReferenceSpec extends Specification with JsonSpec {
  validate("ref")
}
