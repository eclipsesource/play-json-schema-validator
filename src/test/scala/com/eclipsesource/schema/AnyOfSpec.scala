package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class AnyOfSpec extends Specification with JsonSpec {
  validate("anyOf")
  validate("anyOf", "ajv_tests")
}
