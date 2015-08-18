package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.libs.json.{JsNumber, JsString, Json}
import play.api.mvc.{Results, Handler}
import play.api.test._

class RefRemoteSpec extends PlaySpecification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/remotes", path)
  }

    "Remote ref" should {

      "validate" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
        val resourceUrl: URL = getClass.getResource("/draft4/refRemote.json")
        foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
      }
    }

  "change resolution scope" should {

    val schema = JSONSource.schemaFromString(
      """{
        |"id": "http://localhost:1234/",
        |"items": {
        |  "id": "folder/",
        |  "items": {"$ref": "folderInteger.json"}
        |}
        }""".stripMargin).get

    "changed scope ref valid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {

      val data = Json.arr(Json.arr(JsNumber(1)))
      val result = SchemaValidator.validate(schema)(data)
      result.isSuccess must beTrue
    }

    "changed scope ref invalid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val data = Json.arr(Json.arr(JsString("a")))
      val result = SchemaValidator.validate(schema)(data)
      result.isFailure must beTrue
    }
  }

}
