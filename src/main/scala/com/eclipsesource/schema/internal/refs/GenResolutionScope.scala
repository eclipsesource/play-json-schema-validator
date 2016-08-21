package com.eclipsesource.schema.internal.refs

import play.api.libs.json.JsPath

trait GenResolutionContext[A] {
  def refResolver: GenRefResolver[A]
  def scope: GenResolutionScope[A]
  def schemaPath = scope.schemaPath
  def instancePath = scope.instancePath
  val hasBeenVisited = scope.visited.contains _
  def documentRoot = scope.documentRoot
  def currentId = scope.id
}

case class GenResolutionScope[A : CanHaveRef](
                                               documentRoot: A,
                                               id: Option[Pointer] = None,     // current resolution scope
                                               schemaPath: JsPath = JsPath \ "#",
                                               instancePath: JsPath = JsPath,
                                               visited: Set[String] = Set.empty[String] // tracks all visited refs
                                             ) {

  def hasBeenVisited = visited.contains _
  def addVisited(ref: String) = copy(visited = visited + ref)
}

case class DocumentCache[A](private[schema] val mapping: Map[String, A] = Map.empty[String, A]) {

  def add(id: Pointer)(schemaType: A): DocumentCache[A] =
    if (mapping.contains(id.value)) this
    else copy(mapping = mapping + (id.value -> schemaType))

  def get(id: Pointer): Option[A] = mapping.get(id.value)

  def contains(id: Pointer): Boolean = mapping.contains(id.value)
}
