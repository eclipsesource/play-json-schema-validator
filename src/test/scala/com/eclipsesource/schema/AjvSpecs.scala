package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Handler
import play.api.test.{PlaySpecification, WithServer}


class AjvSpecs extends PlaySpecification with JsonSpec {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/remotes", path)
  }

  def createApp = new GuiceApplicationBuilder().routes(routes).build()

  sequential

  "Ajv-based tests" should {
    "restore root after resolve"  in new WithServer(app = createApp, port = 1234) {
      validate("12_restoring_root_after_resolve", "ajv_tests")
    }

    "root ref in ref in remote" in new WithServer(app = createApp, port = 1234) {
      validate("13_root_ref_in_ref_in_remote_ref", "ajv_tests")
    }

    "escape pattern error" in new WithServer(app = createApp, port = 1234) {
      validate("28_escaping_pattern_error", "ajv_tests")
    }

    "dependencies fail" in new WithServer(app = createApp, port = 1234) {
      validate("94_dependencies_fail", "ajv_tests")
    }
  }
}
