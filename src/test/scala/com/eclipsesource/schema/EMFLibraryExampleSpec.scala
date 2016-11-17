package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json.{JsObject, JsResult, Json}

class EMFLibraryExampleSpec extends Specification {

  val librarySchema = """
      |{
      |  "definitions": {
      |    "writer": {
      |      "type": "object",
      |      "properties": {
      |        "name": {"type": "string" },
      |        "books": {
      |          "type": "array",
      |          "items": {
      |            "$ref": "#/definitions/book"
      |          }
      |        }
      |      }
      |    },
      |    "book": {
      |      "type": "object",
      |      "properties": {
      |        "title": {"type": "string"},
      |        "pages": {"type": "number"},
      |        "category": {
      |          "type": "string",
      |          "enum": ["Mystery", "ScienceFiction", "Biography"]
      |        },
      |        "author": {
      |          "$ref": "#/definitions/writer"
      |        }
      |      }
      |    }
      |  },
      |  "properties": {
      |    "name": { "type": "string" },
      |    "writers": {
      |      "type": "array",
      |      "items": {
      |        "$ref": "#/definitions/writer"
      |      }
      |    },
      |    "books": {
      |      "type": "array",
      |      "items": {
      |        "$ref": "#/definitions/book"
      |      }
      |    }
      |  }
      |}
      |""".stripMargin

    "handle multiple required errors" in {
      val result2: JsResult[SchemaType] = JsonSource.schemaFromString(librarySchema)
      val schema = result2.get
      lazy val goethe: JsObject = Json.obj(
        "name" -> "Goethe"
      )
      lazy val faust: JsObject = Json.obj(
        "title" -> "Faust",
        "pages" -> 200,
        "category" -> "Mystery",
        "author" -> goethe
      )
      val result = SchemaValidator().validate(schema)(
        Json.obj(
          "name" -> "Trinity College",
          "writers" -> Json.arr(
            Json.obj("name" -> "John Wayne"),
            goethe
          ),
          "books" -> Json.arr(
            faust
          )
        )
      )
      result.isSuccess must beTrue
  }
}
