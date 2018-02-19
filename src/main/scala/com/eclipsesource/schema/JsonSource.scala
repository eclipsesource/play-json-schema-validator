package com.eclipsesource.schema

import java.io.InputStream
import java.net.URL

import play.api.libs.json._

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Convenience class for obtaining JSON values and Schemas from different sources.
  */
object JsonSource {

  /**
    * Tries to parse the given JSON string.
 *
    * @param json the input string
    * @return the result wrapped in a Try
    */
  def fromString(json: String): Try[JsValue] = Try { Json.parse(json) }

  /**
    * Fetches the content from the given URL and tries to parse the result as JSON.
    *
    * @param url the URL at which a JSON instance is expected
    * @return the result wrapped in a Try
    */
  def fromUrl(url: URL): Try[JsValue] = Try {
    val source = Source.fromURL(url)
    try source.getLines.mkString("\n") finally source.close
  }.flatMap(fromString)


  /**
    * Tries to parse the given JSON string and convert it to a JSON schema.
    *
    * @param json the input string
    * @return the result wrapped in a JsResult
    */
  def schemaFromString(json: String)(implicit reads: Reads[SchemaType]): JsResult[SchemaType] =
    Json.fromJson[SchemaType](Json.parse(json))

  /**
    * Tries to read from the given InputStream and convert the result to a JSON schema.
    * Closes the stream in any case.
    *
    * @param inputStream the input string
    * @return the read result wrapped in a JsResult
    */
  def schemaFromStream(inputStream: InputStream)(implicit reads: Reads[SchemaType]): JsResult[SchemaType] =
    try Json.fromJson[SchemaType](Json.parse(inputStream)) finally inputStream.close()

  /**
    * Fetches the content from the given URL and tries to parse the result as a JSON schema.
    *
    * @param url the URL at which a JSON schema is expected
    * @return the result wrapped in a JsResult
    */
  def schemaFromUrl(url: URL)(implicit reads: Reads[SchemaType]): JsResult[SchemaType] = {
    for {
      schemaJson <- JsonSource.fromUrl(url) match {
        case Success(json) => JsSuccess(json)
        case Failure(throwable) => JsError(throwable.getMessage)
      }
      schema <- Json.fromJson[SchemaType](schemaJson)
    } yield schema
  }
}
