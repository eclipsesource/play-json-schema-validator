package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema._
import com.eclipsesource.schema.drafts.{Version4, Version7}
import com.eclipsesource.schema.internal.draft4.constraints.{ArrayConstraints4, ObjectConstraints4}
import com.eclipsesource.schema.internal._
import com.osinka.i18n.Lang
import org.specs2.mutable.Specification
import play.api.libs.json._

class SchemaRefResolverSpec extends Specification { self =>

  def getResolved(e: Either[JsonValidationError, ResolvedResult]): SchemaType = {
    e.right.get.resolved
  }

  def scopeId(e: Either[JsonValidationError, ResolvedResult]): Option[Ref] = {
    e.right.get.scope.id
  }

  def schemaPath(e: Either[JsonValidationError, ResolvedResult]): Option[String] = {
    e.right.get.scope.schemaPath
  }


  "SchemaRefResolver version 4" should {
    import Version4._

    val resolver = SchemaRefResolver(Version4,
      DocumentCache().add(
        Ref(Version4.SchemaUrl)
      )(JsonSource.schemaFromUrl(self.getClass.getResource("/json-schema-draft-04.json")).get)
    )

    "resolve references into JSON schema v4 draft" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "$ref": "http://json-schema.org/draft-04/schema#"
          |}""".stripMargin).get

      val scope = SchemaResolutionScope(schema)
      val arrayDef = resolver.resolveFromRoot("#/definitions/schemaArray", scope)
      val anyOf = resolver.resolveFromRoot("#/properties/anyOf", scope)
      anyOf.map(_.resolved) must beRight[SchemaType].which(_.isInstanceOf[SchemaArray])
      arrayDef.map(_.resolved) must beRight[SchemaType].which(_.isInstanceOf[SchemaArray])
    }

    "be able to resolve JSON Pointer examples" in {

      val schema = SchemaObject(
        Seq(
          SchemaProp("foo",
            SchemaTuple(
              Seq(
                SchemaValue(JsString("bar")),
                SchemaValue(JsString("baz"))
              ),
              ArrayConstraints4()
            )
          ),
          SchemaProp("", SchemaValue(JsNumber(0))),
          SchemaProp("a/b", SchemaValue(JsNumber(1))),
          SchemaProp("c%d", SchemaValue(JsNumber(2))),
          SchemaProp("e^f", SchemaValue(JsNumber(3))),
          SchemaProp("g|h", SchemaValue(JsNumber(4))),
          SchemaProp("i\\j", SchemaValue(JsNumber(5))),
          SchemaProp("k\'l", SchemaValue(JsNumber(6))),
          SchemaProp(" ", SchemaValue(JsNumber(7))),
          SchemaProp("m~n", SchemaValue(JsNumber(8)))
        ),
        ObjectConstraints4()
      )

      val scope = SchemaResolutionScope(schema)
      resolver.resolveFromRoot("#/foo/0", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsString("bar")))
      resolver.resolveFromRoot("#/", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(0)))
      resolver.resolveFromRoot("#/a~1b", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(1)))
      // TODO: fails
      // resolver.resolveValue("#/c%d", scope)   must beRight[SchemaType](SchemaValue(JsNumber(2)))
      resolver.resolveFromRoot("#/e^f", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(3)))
      resolver.resolveFromRoot("#/g|h", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(4)))
      resolver.resolveFromRoot("#/i\\j", scope).map(_.resolved) must beRight[SchemaType](SchemaValue(JsNumber(5)))
    }

    "resolve JSON pointer" in {
      val schema = JsonSource.schemaFromString(
        """{
          |  "type": "object",
          |  "definitions": {
          |    "foo": {
          |      "$ref": "#/definitions/bar"
          |    },
          |    "bar": {
          |      "minimum": 10
          |    }
          |  },
          |  "properties": {
          |    "n": {
          |      "$ref": "#/definitions/foo"
          |    }
          |  }
          |}""".
          stripMargin).get

      val context = SchemaResolutionScope(schema)
      val result = resolver.resolveFromRoot("#/properties/n", context)
      result must beRight.which(_.resolved.isInstanceOf[SchemaNumber])
    }

    "resolve nil fragments" in {
      val schema = JsonSource.schemaFromString("{}").get
      // scope and schema do not matter in this test
      implicit val lang: Lang = Lang.Default
      val result = resolver.resolveLocal(Nil, SchemaResolutionScope(schema), schema)
      result.toEither must beRight.which(_.resolved == schema)
    }
  }

  "SchemaRefResolver version 7 - section 8.2.4" should {

    import Version7._
    val schema = JsonSource.schemaFromString("""
                                               |{
                                               |  "$id": "http://example.com/root.json",
                                               |  "definitions": {
                                               |    "A": { "$id": "#foo" },
                                               |    "B": {
                                               |      "$id": "other.json",
                                               |      "definitions": {
                                               |        "X": { "$id": "#bar" },
                                               |        "Y": { "$id": "t/inner.json" }
                                               |      }
                                               |    },
                                               |    "C": {
                                               |      "$id": "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"
                                               |    }
                                               |  }
                                               |}""".stripMargin
    ).get

    val resolver = SchemaRefResolver(
      Version7,
      DocumentCache().addAll(collectSchemas(schema, Some(Ref("http://example.com/root.json"))))
    )
    val scope = SchemaResolutionScope(schema)

    "document root" in {
      val a = resolver.resolveFromRoot("http://example.com/root.json", scope)
      val b = resolver.resolveFromRoot("http://example.com/root.json#", scope)
      a.isRight must beTrue
      b.isRight must beTrue

      getResolved(a) must beEqualTo(getResolved(b))
    }

    "#/definitions/A" in {

      val a = resolver.resolveFromRoot("http://example.com/root.json#foo", scope)
      val b = resolver.resolveFromRoot("http://example.com/root.json#/definitions/A", scope)

      a.isRight must beTrue
      b.isRight must beTrue
      getResolved(a) must beEqualTo(getResolved(b))

      scopeId(a) must beSome(Ref("http://example.com/root.json#foo"))
      schemaPath(a) must beNone

      schemaPath(b) must beNone
      scopeId(b) must beSome(Ref("http://example.com/root.json#foo"))
    }

    "#/definitions/B" in {
      val a = resolver.resolveFromRoot("http://example.com/other.json", scope)
      val b = resolver.resolveFromRoot("http://example.com/other.json#", scope)
      val c = resolver.resolveFromRoot("http://example.com/root.json#/definitions/B", scope)

      a.isRight must beTrue
      b.isRight must beTrue
      c.isRight must beTrue

      getResolved(a) must beEqualTo(getResolved(b))
      getResolved(b) must beEqualTo(getResolved(c))

      scopeId(a) must beSome(Ref("http://example.com/other.json"))
      schemaPath(a) must beNone

      scopeId(b) must beSome(Ref("http://example.com/other.json"))
      schemaPath(b) must beSome("#")

      scopeId(c) must beSome(Ref("http://example.com/other.json"))
      //      schemaPath(c) must beSome("#/definitions/B")
      schemaPath(c) must beNone
    }

    "#/definitions/B/definitions/X" in {
      val a = resolver.resolveFromRoot("http://example.com/other.json#bar", scope)
      val b = resolver.resolveFromRoot("http://example.com/other.json#/definitions/X", scope)
      val c = resolver.resolveFromRoot("http://example.com/root.json#/definitions/B/definitions/X", scope)

      a.isRight must beTrue
      b.isRight must beTrue
      c.isRight must beTrue

      getResolved(a) must beEqualTo(getResolved(b))
      getResolved(b) must beEqualTo(getResolved(c))

      scopeId(a) must beSome(Ref("http://example.com/other.json#bar"))
      schemaPath(a) must beNone

      scopeId(b) must beSome(Ref("http://example.com/other.json#bar"))
      schemaPath(b) must beNone

      scopeId(c) must beSome(Ref("http://example.com/other.json#bar"))
      schemaPath(c) must beNone
    }

    "#/definitions/B/definitions/Y" in {
      val a = resolver.resolveFromRoot("http://example.com/t/inner.json", scope)
      val b = resolver.resolveFromRoot("http://example.com/t/inner.json#", scope)
      val c = resolver.resolveFromRoot("http://example.com/other.json#/definitions/Y", scope)
      val d = resolver.resolveFromRoot("http://example.com/root.json#/definitions/B/definitions/Y", scope)

      getResolved(a) must beEqualTo(getResolved(b))
      getResolved(b) must beEqualTo(getResolved(c))
      getResolved(c) must beEqualTo(getResolved(d))
    }

    "#/definitions/C" in {
      val a = resolver.resolveFromRoot("urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f", scope)
      val b = resolver.resolveFromRoot("urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f#", scope)
      val c = resolver.resolveFromRoot("http://example.com/root.json#/definitions/C", scope)

      a.isRight must beTrue
      b.isRight must beTrue
      c.isRight must beTrue

      getResolved(a) must beEqualTo(getResolved(b))
      getResolved(b) must beEqualTo(getResolved(c))
    }
  }
}
