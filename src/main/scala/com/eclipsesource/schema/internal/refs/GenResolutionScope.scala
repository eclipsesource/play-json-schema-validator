package com.eclipsesource.schema.internal.refs

import java.net.URI
import play.api.libs.json.JsPath

import scala.util.Try

case class GenResolutionContext[A : CanHaveRef](
                                                 refResolver: GenRefResolver[A],
                                                 scope: GenResolutionScope[A]
                           ) {
  def schemaPath = scope.schemaPath
  def instancePath = scope.instancePath
  def hasBeenVisited = scope.visited.contains _
  def updateScope(scopeUpdateFn: GenResolutionScope[A] => GenResolutionScope[A]): GenResolutionContext[A] =
    copy(scope = scopeUpdateFn(scope))
  def updateResolutionScope(a: A) = copy(scope = refResolver.updateResolutionScope(scope, a))
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

class GenGlobalContextCache[A : CanHaveRef] {

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
