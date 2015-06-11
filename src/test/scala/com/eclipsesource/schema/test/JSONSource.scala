package com.eclipsesource.schema.test

import java.net.URL

import com.eclipsesource.schema.QBType
import play.api.libs.json.{JsResult, JsValue, Json}

import scala.io.{BufferedSource, Source}
import scala.util.Try

object JSONSource {

  def fromString(json: String): Try[JsValue] = Try { Json.parse(json) }
  
  def schemaFromString(json: String): JsResult[QBType] = Json.fromJson[QBType](Json.parse(json))

  def fromURL(url: URL): Try[JsValue] = {
    val source = Source.fromURL(url)
    val lines = try source.getLines().mkString("\n") finally source.close()
    fromString(lines)
  }

  def fromFile(filePath: String): Try[JsValue] = {
    val source = Source.fromFile(filePath)
    val lines  = try source.getLines().mkString("\n") finally source.close()
    fromString(lines)
  }


}
