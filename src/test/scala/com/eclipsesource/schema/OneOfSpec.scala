package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class OneOfSpec extends Specification with JsonSpec {
  validate("oneOf")
}
