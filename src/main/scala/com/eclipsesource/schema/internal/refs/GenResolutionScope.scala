package com.eclipsesource.schema.internal.refs

import java.net.URI

import com.eclipsesource.schema.Pointer
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

case class PointerToSchemaCache[A : CanHaveRef]() {

  private var urlMapping: Map[String, A] = Map()
  private var idMapping: Map[String, A] = Map()

  def add(url: String)(schemaType: A): A = {

    def isAbsolute = Try { new URI(url) }.map(_.isAbsolute).getOrElse(false)

    if (isAbsolute) {
      urlMapping = urlMapping + (url -> schemaType)
      schemaType
    } else {
      schemaType
    }
  }

  def get(url: String): Option[A] = urlMapping.get(url)

  def addId(id: Pointer)(schemaType: A): A =
    if (idMapping.contains(id.value)) {
      schemaType
    } else {
      idMapping = idMapping + (id.value -> schemaType)
      schemaType
    }

  def getId(id: Pointer): Option[A] = idMapping.get(id.value)
}
