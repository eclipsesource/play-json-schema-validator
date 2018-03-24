package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.drafts.Version4
import org.specs2.mutable.Specification
import play.api.libs.json.{JsValue, Json}

class SimplePerformanceSpec extends Specification {

  import Version4._

  def timed(name: String)(body: => Unit) {
    val start = System.currentTimeMillis()
    body
    println(name + ": " + (System.currentTimeMillis() - start) + " ms")
  }

  val validator = SchemaValidator(Some(Version4))
  val schemaUrl: URL = getClass.getResource("/issue-99-1.json")
  val schema: SchemaType = JsonSource.schemaFromUrl(schemaUrl).get

  val instance: JsValue = Json.parse("""{ "mything": "the thing" }""".stripMargin)

  timed("preloaded") {
    for (_ <- 1 to 1000) validator.validate(schema, instance)
  }
  timed("url based") {
    for (_ <- 1 to 1000) validator.validate(schemaUrl)(instance)
  }

}
