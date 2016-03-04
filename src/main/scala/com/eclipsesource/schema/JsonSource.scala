package com.eclipsesource.schema

import java.net.URL

import play.api.libs.json.{JsResult, JsValue, Json}

import scala.io.Source
import scala.util.Try

object JsonSource {

  def fromString(json: String): Try[JsValue] = Try { Json.parse(json) }
  
  def schemaFromString(json: String): JsResult[SchemaType] = Json.fromJson[SchemaType](Json.parse(json))

  def fromURL(url: URL): Try[JsValue] = Try {
    val source = Source.fromURL(url)
    try source.getLines.mkString("\n") finally source.close
  }.flatMap(fromString)

  def fromFile(filePath: String): Try[JsValue] = Try {
    val source = Source.fromFile(filePath)
    try source.getLines.mkString("\n") finally source.close
  }.flatMap(fromString)


}
