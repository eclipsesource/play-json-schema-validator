package com.eclipsesource.schema.jsonforms

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.validation.VA
import org.specs2.mutable.Specification
import play.api.libs.json._

class JsonFormsSpec extends Specification {

  val jsonFormSchema = JsonSource.schemaFromString("""{
                                                     |  "definitions": {
                                                     |    "control": {
                                                     |      "type": "object",
                                                     |      "properties": {
                                                     |        "type": {
                                                     |          "type": "string",
                                                     |          "enum": [
                                                     |            "Control"
                                                     |          ]
                                                     |        },
                                                     |        "label": {
                                                     |          "type": "string"
                                                     |        },
                                                     |        "scope": {
                                                     |          "type": "object",
                                                     |          "properties": {
                                                     |            "$ref": {
                                                     |              "type": "string"
                                                     |            }
                                                     |          }
                                                     |        }
                                                     |      },
                                                     |      "required": [
                                                     |        "type",
                                                     |        "scope"
                                                     |      ]
                                                     |    },
                                                     |    "layout": {
                                                     |      "type": "object",
                                                     |      "properties": {
                                                     |        "type": {
                                                     |          "type": "string",
                                                     |          "enum": [
                                                     |            "HorizontalLayout",
                                                     |            "VerticalLayout",
                                                     |            "Group"
                                                     |          ]
                                                     |        },
                                                     |        "label": {
                                                     |          "type": "string"
                                                     |        },
                                                     |        "elements": {
                                                     |          "type": "array",
                                                     |          "items": {
                                                     |            "$ref": "#"
                                                     |          }
                                                     |        }
                                                     |      },
                                                     |      "required": [
                                                     |        "type",
                                                     |        "elements"
                                                     |      ]
                                                     |    },
                                                     |    "categorization": {
                                                     |      "type": "object",
                                                     |      "properties": {
                                                     |        "type": {
                                                     |          "type": "string",
                                                     |          "enum": [
                                                     |            "Categorization"
                                                     |          ]
                                                     |        },
                                                     |        "elements": {
                                                     |          "type": "array",
                                                     |          "items": {
                                                     |            "type": "object",
                                                     |            "properties": {
                                                     |              "type": {
                                                     |                "type": "string",
                                                     |                "enum": [
                                                     |                  "Category"
                                                     |                ]
                                                     |              },
                                                     |              "elements": {
                                                     |                "type": "array",
                                                     |                "items": {
                                                     |                  "$ref": "#"
                                                     |                }
                                                     |              },
                                                     |              "label": { "type": "string" }
                                                     |            },
                                                     |            "required": [
                                                     |              "type",
                                                     |              "elements",
                                                     |             "label"
                                                     |            ]
                                                     |          }
                                                     |        }
                                                     |      },
                                                     |      "required": [
                                                     |        "type",
                                                     |        "elements"
                                                     |      ]
                                                     |    }
                                                     |  },
                                                     |  "type": "object",
                                                     |  "oneOf": [
                                                     |    {
                                                     |      "$ref": "#/definitions/categorization"
                                                     |    },
                                                     |    {
                                                     |      "$ref": "#/definitions/layout"
                                                     |    },
                                                     |    {
                                                     |      "$ref": "#/definitions/control"
                                                     |    }
                                                     |  ]
                                                     |}""".stripMargin).get

  "SchemaValidator" should {

    "validate valid instance" in {
      val json = Json.parse("""{
                              |"type": "Group",
                              |"label": "This is a fancy label",
                              |"elements": [{
                              |  "type": "control",
                              |  "elements": [],
                              |  "label": "foo",
                              |  "scope": {
                              |    "$ref": "fodo"
                              |  }
                              |}]
                              |}""".stripMargin)

      val result: JsResult[JsValue] = SchemaValidator().validate(jsonFormSchema, json)
      result.isError must beTrue
      result.asEither must beLeft.like { case error =>
        val JsDefined(subErrors) = error.toJson(0) \ "errors"
        subErrors.as[JsObject].keys.size == 3
      }
    }
  }
}
