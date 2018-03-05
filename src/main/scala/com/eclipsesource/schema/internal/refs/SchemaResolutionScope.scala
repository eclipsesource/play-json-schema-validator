package com.eclipsesource.schema.internal.refs

import com.eclipsesource.schema.internal.constraints.Constraints.{AnyConstraints, HasAnyConstraint}
import com.eclipsesource.schema.{SchemaArray, SchemaObject, SchemaRoot, SchemaTuple, SchemaType}
import play.api.libs.json.JsPath

import scala.collection.mutable

case class SchemaResolutionScope(documentRoot: SchemaType,
                                 id: Option[Ref] = None, // current resolution scope
                                 schemaPath: JsPath = JsPath \ "#",
                                 instancePath: JsPath = JsPath,
                                 depth: Int = 0,
                                 origin: Option[JsPath] = None,
                                 schemaUri: Option[String] = None
                             ) {

  val knownSchemas: Map[Ref, SchemaType] =
    collectSchemas(documentRoot.asInstanceOf[SchemaType], id, Map())

  /**
    * Traverse the given schema and returns a Map of Refs representing the resolution scope of the mapped
    * schema elements.
    * @param schema the schema element to be traversed
    * @param resolutionScope the current resolution scope
    * @param knownSchemas Map containing all found scopes so far
    * @return Map containing all found scopes
    */
  private def collectSchemas(schema: SchemaType, resolutionScope: Option[Ref], knownSchemas: Map[Ref, SchemaType]): Map[Ref, SchemaType] = {

    val currentScope = schema.constraints.id.map(i => Refs.mergeRefs(Ref(i), resolutionScope))
    val updatedMap = currentScope.fold(knownSchemas)(id => knownSchemas + (id -> schema))
    val m = schema match {
      case SchemaObject(props, _, _) => props.foldLeft(updatedMap) {
        (schemas, prop) => {
          collectSchemas(prop.schemaType, currentScope orElse resolutionScope, schemas)
        }
      }
      case SchemaArray(item, _, _) => collectSchemas(item, currentScope, updatedMap)
      case SchemaTuple(items, _, _) => items.foldLeft(updatedMap) {
        (schemas, item) =>  collectSchemas(item, currentScope, schemas)
      }
      case SchemaRoot(_ ,s) => collectSchemas(s, resolutionScope, updatedMap)
      case _ => updatedMap
    }

    schema.constraints.subSchemas.foldLeft(m) {
      (schemas, s) => collectSchemas(s, currentScope orElse resolutionScope, schemas)
    }
  }
}

case class DocumentCache(private[schema] val mapping: collection.concurrent.Map[String, SchemaType] = collection.concurrent.TrieMap.empty[String, SchemaType]) {

  def add(id: Ref)(schemaType: SchemaType) = {
    mapping += (id.value -> schemaType)
    this
  }

  def get(value: String): Option[SchemaType] = mapping.get(value)

  def exists(pred: String => Boolean): Boolean = mapping.keys.exists(pred)
}
