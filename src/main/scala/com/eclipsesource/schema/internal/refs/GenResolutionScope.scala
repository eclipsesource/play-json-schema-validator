package com.eclipsesource.schema.internal.refs

import java.net.URI
import play.api.libs.json.JsPath

import scala.util.Try

trait GenResolutionContext[A] {
  def refResolver: GenRefResolver[A]
  def scope: GenResolutionScope[A]
  def schemaPath = scope.schemaPath
  def instancePath = scope.instancePath
  val hasBeenVisited = scope.visited.contains _
  def documentRoot = scope.documentRoot
  def currentId = scope.id
  def isRootScope = scope.isRootScope
}

case class GenResolutionScope[A : CanHaveRef](
                    documentRoot: A,
                    id: Option[String] = None,     // current resolution scope
                    rootId: Option[String] = None, // base URI
                    schemaPath: JsPath = JsPath \ "#",
                    instancePath: JsPath = JsPath,
                    visited: Set[String] = Set.empty // tracks all visited refs
) {
  def isRootScope = {
    val isRootScope = for {
      scope <- id
      rootScope <- rootId
    } yield scope == rootScope
    isRootScope.getOrElse(false)
  }

  def hasBeenVisited = visited.contains _

  def addVisited(ref: String) = copy(visited = visited + ref)
}

case class GenGlobalContextCache[A : CanHaveRef]() {

  private var cache: Map[String, A] = Map()

  def add(url: String)(schemaType: A): A = {
    def isAbsolute(path: String) = Try { new URI(path) }.map(_.isAbsolute).getOrElse(false)
    if (isAbsolute(url)) {
      cache = cache + (url -> schemaType)
      schemaType
    } else {
      schemaType
    }
  }

  def get(url: String): Option[A] = {
    val cached = cache.get(url)
    cached
  }
}
