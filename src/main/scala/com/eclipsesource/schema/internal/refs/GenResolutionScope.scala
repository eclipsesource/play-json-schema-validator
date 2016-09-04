package com.eclipsesource.schema.internal.refs

import play.api.libs.json.JsPath

trait GenResolutionContext[A] {
  def refResolver: GenRefResolver[A]
  def scope: GenResolutionScope[A]
  def schemaPath = scope.schemaPath
  def instancePath = scope.instancePath
  val hasBeenVisited = scope.visited.contains _
  def currentId = scope.id
}

case class GenResolutionScope[A : CanHaveRef](
                                               documentRoot: A,
                                               id: Option[Ref] = None, // current resolution scope
                                               schemaPath: JsPath = JsPath \ "#",
                                               instancePath: JsPath = JsPath,
                                               visited: Set[Ref] = Set.empty[Ref] // tracks all visited refs,
                                             ) {

  def hasBeenVisited = visited.contains _
  def addVisited(ref: Ref) = copy(visited = visited + ref)
}

case class DocumentCache[A](private[schema] val mapping: Map[String, A] = Map.empty[String, A]) {

  def add(id: Ref)(schemaType: A): DocumentCache[A] = copy(mapping = mapping + (id.value -> schemaType))

  def get(id: Ref): Option[A] = mapping.get(id.value)

  def apply(id: Ref) = mapping(id.value)

  def contains(id: Ref): Boolean = mapping.contains(id.value)
}
