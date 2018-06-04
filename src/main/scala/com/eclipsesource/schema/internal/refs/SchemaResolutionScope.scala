package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.SchemaType
import play.api.libs.json.JsPath
import com.eclipsesource.schema.internal._

case class SchemaResolutionScope(documentRoot: SchemaType,
                                 id: Option[Ref] = None, // current resolution scope
                                 schemaJsPath: Option[JsPath] = None,
                                 instancePath: JsPath = JsPath,
                                 depth: Int = 0,
                                 referrer: Option[JsPath] = None
                                ) {


  def schemaPath: Option[String] = schemaJsPath.map(jsPath => SchemaUtil.dropSlashIfAny(jsPath.toString()))

}

case class DocumentCache(mapping: collection.concurrent.Map[String, SchemaType] = collection.concurrent.TrieMap.empty[String, SchemaType]) {

  def add(id: Ref)(schemaType: SchemaType): DocumentCache = {
    mapping += (id.value -> schemaType)
    this
  }

  def addAll(schemas: Map[String, SchemaType]): DocumentCache = {
    mapping ++= schemas
    this
  }

  def get(ref: Ref): Option[SchemaType] = mapping.get(ref.value)

  def get(s: String): Option[SchemaType] = mapping.get(s)

  def apply(ref: Ref): SchemaType = mapping(ref.value)

  def contains(ref: Ref): Boolean = mapping.keySet.contains(ref.value)
}
