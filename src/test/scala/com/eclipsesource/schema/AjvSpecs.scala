package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import org.specs2.specification.AfterAll
import org.specs2.specification.dsl.Online
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Handler
import play.api.test.{PlaySpecification, TestServer}

class AjvSpecs extends PlaySpecification with JsonSpec with Online with AfterAll {

  override val validator = {
    SchemaValidator().addSchema(
      "http://localhost:1234/scope_foo.json",
      JsonSource.schemaFromString(
        """{
          |  "definitions": {
          |    "bar": { "type": "string" }
          |  }
          |}""".stripMargin).get
    )
  }

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/remotes", path)
  }

  def createApp = new GuiceApplicationBuilder().routes(routes).build()

  lazy val server = TestServer(port = 1234, createApp)

  def afterAll = { server.stop; Thread.sleep(1000) }

  def validateAjv(testName: String) = validate(testName, "ajv_tests")

  sequential
//
  "Validation from remote resources is possible" >> {
      { server.start; Thread.sleep(1000) } must not(throwAn[Exception]) continueWith {
      validateMultiple(
        Seq(
          "5_recursive_references",
          "12_restoring_root_after_resolve",
          "13_root_ref_in_ref_in_remote_ref",
          "62_resolution_scope_change"
        ),
        "ajv_tests"
      )
    }
  }

  validateAjv("1_ids_in_refs")
  validateAjv("17_escaping_pattern_property")
  validateAjv("19_required_many_properties")
  validateAjv("27_recursive_reference")
  validateAjv("28_escaping_pattern_error")
  validateAjv("87_$_property")
  validateAjv("94_dependencies_fail")
  validateAjv("226_json_with_control_chars")

}
