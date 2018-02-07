package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema.{JsonSource, Version4}
import org.specs2.mutable.Specification

class SchemaReadsSpec extends Specification {

  import Version4._

  "Reads" should {

    "not fail with match error (#104)" in {
      val schema = JsonSource.schemaFromString("""
        |{
        |  "someProp": {"type": "sting"}
        |}""".stripMargin)

      schema.isSuccess must beTrue
    }
  }

}
