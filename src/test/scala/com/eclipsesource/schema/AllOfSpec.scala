package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class AllOfSpec extends Specification with JsonSpec {
  validate("allOf")
}

