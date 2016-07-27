package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import org.specs2.specification.AfterAll
import org.specs2.specification.dsl.Online
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Handler
import play.api.test.{PlaySpecification, TestServer}

class AjvSpecs extends PlaySpecification with JsonSpec with Online with AfterAll {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/remotes", path)
  }

  def createApp = new GuiceApplicationBuilder().routes(routes).build()

  lazy val server = TestServer(port = 1234, createApp)

  def afterAll = { server.stop; Thread.sleep(1000) }

  sequential

  "Validation from remote resources is possible" >> {
    {
      {
        server.start; Thread.sleep(1000)
      } must not(throwAn[Exception])
    } continueWith {
      validateMultiple(
        Seq(
          "5_recursive_references",
          "12_restoring_root_after_resolve",
          "13_root_ref_in_ref_in_remote_ref"
        ),
        "ajv_tests"
      )
    }
  }

  validate("1_ids_in_refs", "ajv_tests")
  validate("19_required_many_properties", "ajv_tests")
  validate("27_recursive_reference", "ajv_tests")
  validate("28_escaping_pattern_error", "ajv_tests")
  validate("94_dependencies_fail", "ajv_tests")

}
