package com.eclipsesource.schema.internal

import java.net.URI

import com.eclipsesource.schema.{RefAttribute, SchemaType}
import play.api.libs.json.JsPath

import scala.util.Try

case class ResolutionContext(
                           refResolver: RefResolver,
                           scope: ResolutionScope
                           ) {
  def schemaPath = scope.schemaPath
  def instancePath = scope.instancePath
  def hasBeenVisited = scope.visited.contains _
  def updateScope(scopeUpdateFn: ResolutionScope => ResolutionScope): ResolutionContext =
    copy(scope = scopeUpdateFn(scope))
  def documentRoot = scope.documentRoot
  def currentId = scope.id
  def isRootScope = scope.isRootScope
}


case class ResolutionScope(
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

  def hasBeenVisited = visited.contains _

  def addVisited(ref: RefAttribute) = copy(visited = visited + ref)
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
