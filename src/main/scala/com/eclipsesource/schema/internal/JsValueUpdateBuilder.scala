package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{RuleProvider, QBType}
import play.api.data.mapping.{Success, Rule, Path}
import play.api.libs.json._
import play.api.libs.json.extensions.JsExtensions

import scala.reflect.ClassTag

/**
 * A JsValueProcessor that finds all types and paths for which the matcher evaluates to true and modifies them via the map
 * method. In contrast to the JsTypeMapper this class acts as a builder and allows to specify multiple mappings
 * at once as well as passing in predicates as matching functions.
 *
 * @param schema
 *               a QB schema
 */
case class JsValueUpdateBuilder(schema: QBType, mappings: List[(QBType => Boolean, PartialFunction[JsValue, JsValue])] = List.empty) {

  val processor = new JsValueProcessor(RuleProvider {
    case (qbType, annotations) if mappings.exists(p => p._1(qbType))=> Rule.fromMapping[JsValue, JsValue] { js =>
      val pf = mappings.find(p => p._1(qbType)).get._2
      if (pf.isDefinedAt(js)) {
        Success(pf(js))
      } else {
        Success(js)
      }
    }
  })

  /**
   * Allows to created a modified version of the passed JsObject by passing in
   * partial functions that are called if the desired type is encountered.
   *
   * @param updater
   *              the partial function that describes how to modify the matched type
   * @return a JsResult containing the possibly modified JsObject
   */
  def byType[A <: QBType : ClassTag](updater: PartialFunction[JsValue, JsValue]): JsValueUpdateBuilder = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    val matcher = (q: QBType) => q.getClass.getInterfaces.contains(clazz) || q.getClass == clazz
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
  def byTypeAndPredicate[A <: QBType : ClassTag](predicate: A => Boolean)(updater: PartialFunction[JsValue, JsValue]): JsValueUpdateBuilder = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    val matcher = (q: QBType) =>  ( q.getClass.getInterfaces.contains(clazz) || q.getClass == clazz) && predicate(q.asInstanceOf[A])
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
  def byPredicate(matcher: QBType => Boolean)(updater: PartialFunction[JsValue, JsValue]): JsValueUpdateBuilder =
    new JsValueUpdateBuilder(schema, (matcher -> updater) :: mappings)

  // TODO: can not map onto same type twice -> test
  /**
   * @inheritdoc
   *
   * @param qbType
   *              a qbType
   * @return true, if the QB type is of interest, false otherwise
   */
  def matcher(qbType: QBType): Boolean = mappings.exists(_._1(qbType))

  private def getByType(qbType: QBType) = mappings.find(_._1(qbType))



  /**
   * Executes the mapping.
   *
   * @param input
   *              the JsObject that should be modified
   * @return a JsResult containing the possibly modified JsObject
   */
  def go(input: JsObject): JsObject = {

    processor.process(schema, input, Path(), Seq.empty).get.asInstanceOf[JsObject]
  }
}
