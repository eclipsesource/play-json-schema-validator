package com.eclipsesource.schema

import java.net.URL
import org.specs2.mutable.Specification

class ExamplesSpec extends Specification {

  val validator = SchemaValidator()
  val resourceUrl: URL = getClass.getResource("/test-schemas/swagger-2.0")

  def validateExample(url: String) = {
    val minimalPetStore = getClass.getResource(url)
    val schema   = JsonSource.schemaFromUrl(resourceUrl)
    val instance = JsonSource.fromUrl(minimalPetStore)
    val result   = validator.validate(schema.get)(instance.get)
    result.isSuccess must beTrue
    result.get must beEqualTo(instance.get)
  }

  "Validator" should {
    "validate petstore-minimal" in {
      validateExample("/test-schemas/petstore-minimal.json")
    }

    "validate petstore-simple" in {
      validateExample("/test-schemas/petstore-simple.json")
    }

    "validate petstore-expanded" in {
      validateExample("/test-schemas/petstore-expanded.json")
    }

    "validate petstore-with-external-docs" in {
      validateExample("/test-schemas/petstore-with-external-docs.json")
    }

    "validate petstore" in {
      validateExample("/test-schemas/petstore.json")
    }
  }

}
