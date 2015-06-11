package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{QBArray, QBType}
import com.eclipsesource.schema.internal.{RefResolver, Context, JsValueProcessor}
import com.eclipsesource.schema.test.JSONSource
import controllers.Assets
import org.specs2.mutable.Specification
import play.api.data.mapping.Path
import play.api.libs.json._
import play.api.test._
import play.api.mvc.Handler

class ResolveRefSpec extends Specification {


  "Relative ref" should {

    //    "be resolvable" in {
    //      val schema = obj(
    //        "properties" -> obj(
    //          "foo" -> qbInteger
    //        )
    //      )
    //      val context = Context(Path, schema, Seq.empty, Set.empty)
    //      val processor = JsValueProcessor(annotationRule)
    //      val resolved: Option[QBType] = processor.resolveRef("#/properties/foo", context)
    //      resolved must beSome
    //    }
    //
    //    "be resolvable when chained" in {
    //      val schema = obj(
    //        "properties" -> obj(
    //          "foo" -> qbInteger,
    //          "bar" -> $ref("#/properties/foo"),
    //          "baz" -> $ref("#/properties/bar")
    //        )
    //      )
    //
    //      val context = Context(Path, schema, Seq.empty, Set.empty)
    //      val processor = JsValueProcessor(annotationRule)
    //      val resolved: Option[QBType] = processor.resolveRef("#/properties/baz", context)
    //      resolved must beSome.which(qbType => qbType.isInstanceOf[QBInteger])
    //    }
    //
    //    "reads test case" in {
    //      val schema = JSONSource.schemaFromString(
    //        """{
    //          |"definitions": {
    //          |  "schemaArray": {
    //          |    "type": "array",
    //          |    "minItems": 1,
    //          |    "items": { "$ref": "#" }
    //          |  }
    //          |}
    //        }""".stripMargin).get
    //
    //      true must beTrue
    //    }

    //    "reads test case 2" in {
    //      val schema = JSONSource.schemaFromString(
    //        """{
    //          |"definitions": {
    //          |  "positiveIntegerDefault0": {
    //          |    "allOf": [ { "$ref": "#/definitions/positiveInteger" }, { "default": 0 } ]
    //          |  }
    //          |},
    //          |"properties": {
    //          |  "minLength": { "$ref": "#/definitions/positiveIntegerDefault0" },
    //          |  "minProperties": { "$ref": "#/definitions/positiveIntegerDefault0" }
    //          |}
    //        }""".stripMargin).get
    //
    //      val context = Context(Path, schema, Seq.empty, Set.empty)
    //      val processor = JsValueProcessor(annotationRule)
    //      val resolved: Option[QBType] = processor.resolveRef("#/properties/minProperties", context)
    //      resolved must beSome
    //      true must beTrue
    //    }



//    "foo" should {
//      val schema = JSONSource.schemaFromString(
//        """{
//          |"definitions": {
//          |  "schemaArray": {
//          |    "type": "array",
//          |    "minItems": 1,
//          |    "items": { "$ref": "#" }
//          |  }
//          |},
//          |"properties": {
//          |  "anyOf": {
//          |    "$ref": "#/definitions/schemaArray"
//          |  }
//          |}
//          |}
//        """.stripMargin).get
//      "bar" in {
//        val context = Context(Path, schema, Seq.empty, Set.empty)
//        val resolved = RefResolver.resolveRef("#/properties/anyOf", context)
//        println(resolved)
//        true must beTrue
//      }
//    }
//
//    val routes: PartialFunction[(String, String), Handler] = {
//      case (_, path) => Assets.versioned("/remotes", path)
//    }
//
//    "RefResolver" should {
//      "resolve ref to another ref" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
//        val schema = JSONSource.schemaFromString("""{
//          "$ref": "http://localhost:1234/subSchemas.json#/refToInteger"
//        }""").get
//
//
//        val context = Context(Path, schema, Seq.empty, Set.empty)
//        val resolved: QBType = RefResolver.replaceRefs(schema, context)
//        println(resolved)
//        true must beTrue
//        //        resolved must beSome.which(t => t.isInstanceOf[QBInteger])
//      }
//    }

    "foo2" should {
      val schema = JSONSource.schemaFromString(
        """{
          |"definitions": {
          |  "schemaArray": {
          |    "type": "array",
          |    "minItems": 1,
          |    "items": { "$ref": "#" }
          |  },
          |  "positiveInteger": {
          |    "type": "integer",
          |    "minimum": 0
          |   },
          |  "positiveIntegerDefault0": {
          |    "allOf": [ { "$ref": "#/definitions/positiveInteger" }, { "default": 0 } ]
          |  }
          |},
          |"properties": {
          |  "minLength": { "$ref": "#/definitions/positiveIntegerDefault0" }
          |}
        }""".stripMargin).get

//      "bar2" in {
//        val context = Context(Path, schema, Seq.empty, Set.empty)
//        val resolved = RefResolver.resolveRef("#/properties/minLength", context)
//        val resolved2 = RefResolver.resolveRef("#/definitions/positiveIntegerDefault0", context)
//        println(resolved)
//        println(resolved2)
//        true must beTrue
//      }
    }

    val schema = JSONSource.schemaFromString(
      """{
        |"$ref": "http://json-schema.org/draft-04/schema#"
                }""".stripMargin).get

    println(Json.prettyPrint(Json.toJson(schema)))

    "be resolvable via " in {

      val context = Context(Path, schema, Seq.empty, Set.empty)
      val updatedRoot = RefResolver.replaceRefs(schema, context)
      val resolved: Option[QBType] = RefResolver.resolveRef("#/definitions/schemaArray", context.copy(root = updatedRoot))
      println(resolved)
      resolved must beSome.which(t => t.isInstanceOf[QBArray])
    }


    "resolve ref" in {
      val context = Context(Path, schema, Seq.empty, Set.empty)
      val updatedRoot = RefResolver.replaceRefs(schema, context)
      val resolved = RefResolver.resolveRef("#/properties/anyOf", context.copy(root = updatedRoot))
      println(resolved.map(q => q.asInstanceOf[QBArray].qbTypes))
      resolved must beSome.which(t => t.isInstanceOf[QBArray])
    }

  }

}
