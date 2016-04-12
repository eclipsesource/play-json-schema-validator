package com.eclipsesource.schema.test

import java.net.URL

import com.eclipsesource.schema._
import org.specs2._
import execute._
import play.api.data.validation.ValidationError
import specification.core._
import org.specs2.specification.dsl.mutable.FragmentBuilder
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class JsonSchemaSpec(description: String, schema: SchemaType, tests: Seq[JsonSchemaTest])
case class JsonSchemaTest(description: String, data: JsValue, valid: Boolean)
case class SpecResult(description: String, valid: Boolean, error: Option[Seq[(JsPath, Seq[ValidationError])]])

trait JsonSpec extends FragmentBuilder {

  val spec = new org.specs2.mutable.Specification {}
  import spec._

  def validate(name: String): Fragments =
    try addFragments(validateFragments(name))
    catch { case e: Exception =>
      addFragments(Fragments(br, Fragment(Text(s"Could not create examples for $name"), Execution.executed(Skipped(e.getMessage)))))
    }

  def validateFragments(name: String): Fragments =
    s2"""|$name should be ok $p
         |${examplesFromUrl(getClass.getResource(s"/draft4/$name.json"))}""".stripMargin

  def examplesFromUrl(url: URL): Fragments = {
    val results: Either[String, Fragments] = fromUrl(url).right.map { specs =>
      Fragments.foreach(specs) { case (specName, rs) =>
        val examples = Fragments.foreach(rs) { result =>
          s2"""  ${result.description ! test(specName, result)}""".stripMargin
       }

       s2"""|$specName $br
            |$examples""".stripMargin
      }
    }
    results.right.getOrElse(Fragments("Spec init" ! Failure(s"Could not read specs from $url.")))
  }

  def test(specName: String, result: SpecResult) =
    if (result.valid) Success(result.description)
    else              Failure(s"'${result.description}' of spec '$specName' failed.")

  def fromUrl(url: URL): Either[String, Seq[(String, Seq[SpecResult])]] = {
    JsonSource.fromURL(url).getOrElse(Failure(s"Could not read JSON from $url.")) match {
      case JsArray(specs) => Right(executeSpecs(specs))
      case json =>
        Left(s"URL $url does not contain any specs or has wrong format. See https://github.com/json-schema/JSON-Schema-Test-Suite for correct format")
    }
  }

  private def executeSpecs(jsonSpecs: Seq[JsValue]): Seq[(String, Seq[SpecResult])] = {
    val specs: Seq[JsonSchemaSpec] = collectSpecs(jsonSpecs)
    specs.map(spec => (spec.description, executeSpec(spec)))
  }

  private def executeSpec(spec: JsonSchemaSpec): Seq[SpecResult] = {
    val schema = spec.schema
    spec.tests.map(spec => {
      val result = SchemaValidator.validate(schema)(spec.data)
      SpecResult(spec.description,
        result.isSuccess == spec.valid,
        result.asEither.left.toOption
      )
    })
  }

  private def collectSpecs(specs: Seq[JsValue]): Seq[JsonSchemaSpec] = {
    specs.map {
      case obj@JsObject(props) => JsonSchemaSpec(
        (obj \ "description").as[String],
        (obj \ "schema").as[SchemaType],
        (obj \ "tests").as[Seq[JsonSchemaTest]]
      )
    }
  }

  implicit lazy val jsonSchemaTestReader: Reads[JsonSchemaTest] = {
    ((__ \ "description").read[String] and
      (__ \ "data").read[JsValue] and
      (__ \ "valid").read[Boolean])
      .tupled.map { case (desc, data, valid) => JsonSchemaTest(desc, data, valid) }
  }
}
