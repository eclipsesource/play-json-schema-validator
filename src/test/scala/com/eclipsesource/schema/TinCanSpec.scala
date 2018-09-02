package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json.{JsValue, Json}

import scala.util.Try

class TinCanSpec extends Specification { self =>

  import com.eclipsesource.schema.drafts.Version4

  val instance = JsonSource.fromString(
    """
      |{
      |  "actor": {
      |    "name": "Sally Glider",
      |    "mbox": "mailto:sally@example.com"
      |  },
      |  "verb": {
      |    "id": "http://adlnet.gov/expapi/verbs/experienced",
      |    "display": { "en-US": "experienced" }
      |  },
      |  "object": {
      |    "id": "http://example.com/activities/solo-hang-gliding",
      |    "definition": {
      |      "name": { "en-US": "Solo Hang Gliding" }
      |    }
      |  }
      |}
    """.stripMargin).get


  "Tin Can Spec" should {

    import Version4._

    def readSchema(filename: String) =
      JsonSource.schemaFromStream(self.getClass.getResourceAsStream(s"/tincan/$filename.json")).get

    "validate basic statement object" in {

      val validator = SchemaValidator(Some(Version4))
        .addSchema("#agent", readSchema("agent"))
        .addSchema("#group", readSchema("group"))
        .addSchema("#inversefunctional", readSchema("inversefunctional"))
        .addSchema("#mbox", readSchema("mbox"))
        .addSchema("#statement_base", readSchema("statement_base"))
        .addSchema("#statement_object", readSchema("statement_object"))
        .addSchema("#verb", readSchema("verb"))
        .addSchema("#languagemap", readSchema("languagemap"))
        .addSchema("#activity", readSchema("activity"))
        .addSchema("#activity_definition", readSchema("activity_definition"))
        .addSchema("#activityid", readSchema("activityid"))

      val result = validator.validate(readSchema("statement_base"), instance)
      result.isSuccess must beTrue
    }
  }
}
