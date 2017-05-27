package com.eclipsesource.schema.internal.refs

import play.api.libs.json.JsPath

trait GenResolutionContext[A] {
  def refResolver: GenRefResolver[A]
  def scope: GenResolutionScope[A]
  def schemaPath: JsPath = scope.schemaPath
  def instancePath: JsPath = scope.instancePath
  val hasBeenVisited: (Ref) => Boolean = scope.visited.contains _
}

case class GenResolutionScope[A : CanHaveRef](documentRoot: A,
                                              id: Option[Ref] = None, // current resolution scope
                                              schemaPath: JsPath = JsPath \ "#",
                                              instancePath: JsPath = JsPath,
                                              visited: Set[Ref] = Set.empty[Ref], // tracks all visited refs
                                              depth: Int = 0,
                                              origin: Option[JsPath] = None) {

  def hasBeenVisited: (Ref) => Boolean = visited.contains
  def addVisited(ref: Ref): GenResolutionScope[A] = copy(visited = visited + ref)
}

case class DocumentCache[A](private[schema] val mapping: Map[String, A] = Map.empty[String, A]) {

  def add(id: Ref)(schemaType: A): DocumentCache[A] = copy(mapping = mapping + (id.value -> schemaType))

  def get(id: Ref): Option[A] = mapping.get(id.value)

  def exists(pred: String => Boolean): Boolean = mapping.keys.exists(pred)
}
