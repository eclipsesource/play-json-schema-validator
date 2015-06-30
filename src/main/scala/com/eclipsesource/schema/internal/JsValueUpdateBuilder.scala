package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{Validator, SchemaType}
import play.api.data.mapping.Path
import play.api.libs.json._

import scala.reflect.ClassTag

/**
 * A JsValueProcessor that finds all types and paths for which the matcher evaluates to true and modifies them via the map
 * method. In contrast to the JsTypeMapper this class acts as a builder and allows to specify multiple mappings
 * at once as well as passing in predicates as matching functions.
 *
 * @param schema
 *               a schema
 */
case class JsValueUpdateBuilder(schema: SchemaType, mappings: List[(SchemaType => Boolean, PartialFunction[JsValue, JsValue])] = List.empty) {

  /**
   * Allows to created a modified version of the passed JsObject by passing in
   * partial functions that are called if the desired type is encountered.
   *
   * @param updater
   *              the partial function that describes how to modify the matched type
   * @return a JsResult containing the possibly modified JsObject
   */
  def byType[A <: SchemaType : ClassTag](updater: PartialFunction[JsValue, JsValue]): JsValueUpdateBuilder = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    val matcher = (q: SchemaType) => q.getClass.getInterfaces.contains(clazz) || q.getClass == clazz
    new JsValueUpdateBuilder(schema, (matcher -> updater) :: mappings)
  }

  /**
   * Allows to created a modified version of the passed JsObject by passing in
   * partial functions that are called if the desired type is encountered.
   *
   * @param updater
   *              the partial function that describes how to modify the matched type
   * @return a JsResult containing the possibly modified JsObject
   */
  def byTypeAndPredicate[A <: SchemaType : ClassTag](predicate: A => Boolean)(updater: PartialFunction[JsValue, JsValue]): JsValueUpdateBuilder = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    val matcher = (q: SchemaType) =>  ( q.getClass.getInterfaces.contains(clazz) || q.getClass == clazz) && predicate(q.asInstanceOf[A])
    new JsValueUpdateBuilder(schema, (matcher -> updater) :: mappings)
  }

  /**
   * Allows to created a modified version of the passed JsObject by passing in
   * partial functions that are called if the matcher evaluates to true
   *
   * @param updater
   *              the partial function that describes how to modify the matched type
   * @return a JsResult containing the possibly modified JsObject
   */
  def byPredicate(matcher: SchemaType => Boolean)(updater: PartialFunction[JsValue, JsValue]): JsValueUpdateBuilder =
    new JsValueUpdateBuilder(schema, (matcher -> updater) :: mappings)

  // TODO: can not map onto same type twice -> test
  /**
   * @inheritdoc
   *
   * @param schemaType
   *              a schema
   * @return true, if the schema type is of interest, false otherwise
   */
  def matcher(schemaType: SchemaType): Boolean = mappings.exists(_._1(schemaType))

  /**
   * Executes the mapping.
   *
   * @param input
   *              the JsObject that should be modified
   * @return a JsResult containing the possibly modified JsObject
   */
  def go(input: JsObject): JsObject = {
    Validator.process(schema, input, Context(Path, schema, Seq.empty, Set.empty)).get.asInstanceOf[JsObject]
  }
}
