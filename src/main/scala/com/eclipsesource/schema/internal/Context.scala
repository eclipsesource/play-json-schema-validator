package com.eclipsesource.schema.internal

import com.eclipsesource.schema.SchemaType
import play.api.data.mapping.Path


case class Context(
                    documentRoot: SchemaType,
                    id: Option[String] = None, // current resolution scope
                    rootId: Option[String] = None, // base URI
                    schemaPath: Path = Path(),
                    instancePath: Path = Path(),
                    visited: Set[String] = Set.empty // tracks all visited refs
) {
  def isRootScope = {
    val isRootScope =  for {
      scope <- id
      rootScope <- rootId
    } yield scope == rootScope
    isRootScope.getOrElse(false)
  }
}

object GlobalContextCache {

  var cache: Map[String, SchemaType] = Map()

  def clear() = {
//    println("CACHE CLEAR")
    cache = Map.empty
  }

  def add(url: String)(schemaType: SchemaType) = {
//    println(s"ADD TO CACHE $url -> ${schemaType.prettyPrint}")
    cache = cache + (url -> schemaType)
    schemaType
  }

  def get(url: String) = {
//    println(s"ASK CACHE for $url/$cache")
    val cached = cache.get(url)
//    cached.foreach(x => println(s"CACHE HIT $x"))
    cached
  }
}