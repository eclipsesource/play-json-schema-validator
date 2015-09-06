package com.eclipsesource.schema.test

import java.io.InputStream
import java.net.URL

import com.eclipsesource.schema._
import org.specs2.execute.Result
import org.specs2.matcher.ThrownExpectations
import org.specs2.specification.Example
import play.api.data.mapping.{VA, Path, ValidationError}
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util.Try

case class JsonSchemaSpec(description: String, schema: SchemaType, tests: Seq[JsonSchemaTest])
case class JsonSchemaTest(description: String, data: JsValue, valid: Boolean)
case class SpecResult(description: String, valid: Boolean, error: Option[Seq[(Path, Seq[ValidationError])]])

object JsonSpec extends ThrownExpectations {

  def examplesFromUrl(url: URL): Seq[Example] = {
    val results: Either[String, Seq[Example]] = fromUrl(url).right.map(specs => {
      for {
        spec <- specs
        test <- spec._2
      } yield {
        if (test.valid) {
          Example(spec._1, success(test.description))
        } else {
          Example(spec._1, failure(s"'${test.description}' of spec '${spec._1}' failed."))
        }
      }
    })
    results.right.getOrElse(Seq(Example("Spec init", failure(s"Could not read specs from $url."))))
  }

  def fromUrl(url: URL): Either[String, Seq[(String, Seq[SpecResult])]] = {
    JsonSource.fromURL(url).getOrElse(failure(s"Could not read JSON from $url.")) match {
      case JsArray(specs) => Right(executeSpecs(specs))
      case json => Left(s"URL $url does not contain any specs or has wrong format. See https://github.com/json-schema/JSON-Schema-Test-Suite for correct format")
    }
  }

  def fromFile(filePath: String): Either[String, Seq[(String, Seq[SpecResult])]] = {
    JsonSource.fromFile(filePath).getOrElse(failure(s"Could not read JSON from $filePath.")) match {
      case JsArray(specs) => Right(executeSpecs(specs))
      case json => Left(s"File $filePath does not contain any specs or has wrong format. See https://github.com/json-schema/JSON-Schema-Test-Suite for correct format.")
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
      .tupled.map { case (desc, data, valid) => JsonSchemaTest(desc, data, valid)}
  }
}