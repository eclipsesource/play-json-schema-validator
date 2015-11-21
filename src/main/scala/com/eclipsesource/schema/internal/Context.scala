package com.eclipsesource.schema.internal

import java.net.URL

import com.eclipsesource.schema.{SchemaType}
import play.api.data.mapping.Path


case class Context(
  schemaPath: Path,
  instancePath: Path,
  root: SchemaType,
  visited: Set[String],
  id: Option[String] = None,
  baseUrl: Option[URL] = None
)

object GlobalContextCache {

  var cache: Map[String, SchemaType] = Map()

  def clear() = {
    cache = Map.empty
  }

  def add(url: String, schemaType: SchemaType) = {
    cache = cache + (url -> schemaType)
    schemaType
  }

  def get(url: String) = {
    val result = cache.get(url)
    println(s"Found cached $result")
    result
  }
}