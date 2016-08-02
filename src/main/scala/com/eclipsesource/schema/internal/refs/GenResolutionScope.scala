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
  def addVisited(ref: String) = {
    copy(visited = visited + ref)
  }
}

case class SchemaCache[A](private[schema] val idMapping: Map[String, A] = Map.empty[String, A]) {

  def addId(id: Pointer)(schemaType: A): SchemaCache[A] =
    if (idMapping.contains(id.value)) {
      this
    }
    else {
      if (id.isAbsolute) copy(idMapping = idMapping + (id.documentName.value -> schemaType))
      else copy(idMapping = idMapping + (id.value -> schemaType))
    }

  def getId(id: Pointer): Option[A] =
    if (id.isAbsolute) idMapping.get(id.documentName.value)
    else idMapping.get(id.value)

  def contains(id: Pointer): Boolean = idMapping.contains(id.value)
}
