package com.eclipsesource.schema

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import org.specs2.mutable.Specification
import java.net.URL

import play.api.libs.json.{JsBoolean, Json, JsNumber}

class TypeSpec  extends Specification {

  "Type" should {
    "validate" in {
      val resourceUrl: URL = getClass.getResource("/draft4/type.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }



  "object type matches objects" should {
    val schema = JSONSource.schemaFromString("""{"type": ["integer", "string"]}""").get

    "an boolean is invalid" in {
      val data = JsBoolean(true)
      val res = SchemaValidator.validate(schema, data)
      res.isFailure must beTrue
    }
  }
}

