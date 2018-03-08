package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema._
import com.eclipsesource.schema.drafts.Version4
import com.eclipsesource.schema.internal.draft4.constraints.{ArrayConstraints4, ObjectConstraints4}
import com.eclipsesource.schema.test.Assets
import com.osinka.i18n.Lang
import org.specs2.mutable.Specification
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._

class SchemaRefResolverSpec extends Specification { self =>

  import Version4._

  def createApp: Application = new GuiceApplicationBuilder()
    .routes(Assets.routes(getClass)).build()

  val resolver = SchemaRefResolver(Version4)

  "SchemaRefResolver" should {

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
      result.toEither must beRight.which(_.resolved == schema )
    }
  }
}
