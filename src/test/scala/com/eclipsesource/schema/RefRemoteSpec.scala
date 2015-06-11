package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.internal.{Context, RefResolver}
import com.eclipsesource.schema.test.{JSONSource, JsonSpec}
import controllers.Assets
import play.api.data.mapping.Path
import play.api.libs.json.{JsString, JsArray, Json, JsNumber}
import play.api.mvc.Handler
import play.api.test._

object RefRemoteSpec extends PlaySpecification {

    val routes: PartialFunction[(String, String), Handler] = {
      case (_, path) => Assets.versioned("/remotes", path)
    }

//  "Remote ref" should {
//
//    "validate" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
//      val resourceUrl: URL = getClass.getResource("/draft4/refRemote.json")
//      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
//    }
//  }

  "change resolution scope" should {

      // TODO: deserialized schema is wrong
      val schema = JSONSource.schemaFromString(
        """{
          |"id": "http://localhost:1234/",
          |"items": {
          |  "id": "folder/",
          |  "items": {"$ref": "folderInteger.json"}
          |}
        }""".stripMargin).get

    println(Json.prettyPrint(Json.toJson(schema)))

    "changed scope ref valid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {


//      val x = RefResolver.fetchSchema("http://localhost:1234/folder/folderInteger.json", Context(Path, schema, Seq.empty, Set.empty))
//      println("x is " + x)

      val data = Json.arr(Json.arr(JsNumber(1)))
      val result = Validator.validate(schema)(data)
      println(result)
      result.isSuccess must beTrue
    }

    "changed scope ref invalid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val data = Json.arr(Json.arr(JsString("a")))
      val result = Validator.validate(schema)(data)
      println(result)
      result.isFailure must beTrue
    }
  }

//  "ref within remote ref" should {
//
//      val schema = JSONSource.schemaFromString("""{
//        "$ref": "http://localhost:1234/subSchemas.json#/refToInteger"
//      }""").get
//
//    "ref within ref valid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
//
//      val data = JsNumber(1)
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isSuccess must beTrue
//    }
//
//    "ref within ref invalid" in  new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234)  {
//      val data = JsString("a")
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isFailure must beTrue
//    }
//
//  }

  "remote ref" should {
//    "remote ref valid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
//      {
//        val schema = JSONSource.schemaFromString("""{"$ref": "http://localhost:1234/integer.json"}""").get
//        val data = JsNumber(1)
//        val result = Validator.validate(schema)(data)
//        println()
//        result.isSuccess must beTrue
//      }
//    }

//    "remote ref invalid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
//      {
//        val schema = JSONSource.schemaFromString("""{"$ref": "http://localhost:1234/integer.json"}""").get
//        val data = JsString("a")
//        val result = Validator.validate(schema)(data)
//        result.isFailure must beTrue
//      }
//    }
  }
//
//  "fragment within remote ref" should {
//    "remote fragment valid" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
//
//      val schema = JSONSource.schemaFromString("""{"$ref": "http://localhost:1234/subSchemas.json#/integer"}""").get
//      val data = JsNumber(1)
//      val result = Validator.validate(schema)(data)
//      println(result)
//      result.isSuccess must beTrue
//    }
//  }

}
