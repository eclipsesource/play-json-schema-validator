package com.eclipsesource.schema

import java.io.InputStream
import java.net.URL

import play.api.libs.json.{JsResult, JsValue, Json}

import scala.io.Source
import scala.util.Try

object JsonSource {

  def fromString(json: String): Try[JsValue] = Try { Json.parse(json) }
  
  def schemaFromString(json: String): JsResult[SchemaType] = Json.fromJson[SchemaType](Json.parse(json))

  def schemaFromStream(inputStream: InputStream): JsResult[SchemaType] =
    Json.fromJson[SchemaType](Json.parse(inputStream))

  def fromURL(url: URL): Try[JsValue] = Try {
    val source = Source.fromURL(url)
    try source.getLines.mkString("\n") finally source.close
  }.flatMap(fromString)

}
