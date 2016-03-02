package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification
import java.net.URL

class NotSpec extends Specification with JsonSpec {

  validate("not")

}
