package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import play.api.test.PlaySpecification

class AjvSpecs extends PlaySpecification with JsonSpec {

  import Version4._
  implicit val validator = SchemaValidator(Version4)
  def validateAjv(testName: String) = validate(testName, "ajv_tests")

  validateAjv("1_ids_in_refs")
  validateAjv("2_root_ref_in_ref")
  validateAjv("17_escaping_pattern_property")
  validateAjv("19_required_many_properties")
  validateAjv("20_failing_to_parse_schema")
  validateAjv("27_recursive_reference")
  validateAjv("27_1_recursive_raml_schema")
  validateAjv("28_escaping_pattern_error")
  validateAjv("33_json_schema_latest")
  validateAjv("63_id_property_not_in_schema")
  validateAjv("70_1_recursive_hash_ref_in_remote_ref")
  validateAjv("70_swagger_schema")
  validateAjv("87_$_property")
  validateAjv("94_dependencies_fail")
  validateAjv("170_ref_and_id_in_sibling")
  validateAjv("226_json_with_control_chars")

}
