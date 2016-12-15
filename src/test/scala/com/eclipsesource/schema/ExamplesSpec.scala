package com.eclipsesource.schema

import org.specs2.mutable.Specification

class ExamplesSpec extends Specification {

  val validator = SchemaValidator()
  val swaggerSchemaUrl = "/test-schemas/swagger-2.0"

  def validateExample(schema: String, url: String) = {
    val schemaUrl = getClass.getResource(url)
    val instanceUrl = getClass.getResource(url)
    val schema   = JsonSource.schemaFromUrl(schemaUrl)
    val instance = JsonSource.fromUrl(instanceUrl)
    val result   = validator.validate(schema.get)(instance.get)
    result.isSuccess must beTrue
    result.get must beEqualTo(instance.get)
  }

  "Validator" should {
    "validate petstore-minimal" in {
      validateExample(swaggerSchemaUrl, "/test-schemas/petstore-minimal.json")
    }

    "validate petstore-simple" in {
      validateExample(swaggerSchemaUrl, "/test-schemas/petstore-simple.json")
    }

    "validate petstore-expanded" in {
      validateExample(swaggerSchemaUrl, "/test-schemas/petstore-expanded.json")
    }

    "validate petstore-with-external-docs" in {
      validateExample(swaggerSchemaUrl, "/test-schemas/petstore-with-external-docs.json")
    }

    "validate petstore" in {
      validateExample(swaggerSchemaUrl, "/test-schemas/petstore.json")
    }

    "validate core schema agsinst itself" in {
      validateExample("/test-schemas/schema", "/test-schemas/schema")
    }
  }

}
