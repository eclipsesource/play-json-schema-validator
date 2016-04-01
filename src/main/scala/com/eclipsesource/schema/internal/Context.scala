package com.eclipsesource.schema.internal

import java.net.URI

import com.eclipsesource.schema.{RefAttribute, SchemaType}
import play.api.libs.json.JsPath

import scala.util.Try


case class Context(
                    documentRoot: SchemaType,
                    id: Option[String] = None,     // current resolution scope
                    rootId: Option[String] = None, // base URI
                    schemaPath: JsPath = JsPath \ "#",
                    instancePath: JsPath = JsPath,
                    visited: Set[RefAttribute] = Set.empty // tracks all visited refs
) {
  def isRootScope = {
    val isRootScope = for {
      scope <- id
      rootScope <- rootId
    } yield scope == rootScope
    isRootScope.getOrElse(false)
  }
}

object GlobalContextCache {

  private var cache: Map[String, SchemaType] = Map()

  def add(url: String)(schemaType: SchemaType): SchemaType = {
    def isAbsolute(path: String) = Try { new URI(path) }.map(_.isAbsolute).getOrElse(false)
    if (isAbsolute(url)) {
      cache = cache + (url -> schemaType)
      schemaType
    } else {
      schemaType
    }
  }

  def get(url: String) = {
    val cached = cache.get(url)
    cached
  }
}