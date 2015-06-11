package com.eclipsesource.schema.internal

import java.util.regex.Pattern

import com.eclipsesource.schema
import com.eclipsesource.schema._
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.{Failure => _, Source => _, Success => _, _}

case class JsValueProcessor(ruleProvider: ((QBType, Seq[QBAnnotation])) => Rule[JsValue, JsValue]) {

  /**
   * Processor dispatch method.
   *
   * @param schema
   *             the current schema
   * @param input
   *             the current JsValue
   * @return a JsResult containing a result of type O
   */
  def process(schema: QBType, input: JsValue, context: Context): VA[JsValue] = {
    (input, schema) match {
      case (json, qbObject: QBClass) =>
        ObjectValidator.validateObject(qbObject, json, context)
//        atObject(qbObject, json, context)
      case (jsArray: JsArray, qbArray: QBArray) =>
        atArray(qbArray, jsArray, context)
      case (jsArray: JsArray, qbTuple: QBTuple) =>
        atTuple(qbTuple, jsArray, context)
      case (j: JsNumber, n: QBNumber) =>
        validate(schema, input, context)
      case (j: JsNumber, i: QBInteger) =>
        validate(schema, input, context)
      case (j: JsBoolean, b: QBBoolean) =>
        validate(schema, input, context)
      case (j: JsString, s: QBString) =>
        validate(schema, input, context)
      case (JsNull, q: QBNull) =>
        validate(schema, input, context)
      case (_: JsUndefined, _) =>
        validate(schema, input, context)
      case _ =>
        Failure(List(context.path -> List(ValidationError("qb.diff.types", Json.obj("schema" -> schema.prettyPrint, "instance" -> input)))))
    }
  }

  def validate(schema: QBType, input: => JsValue, context: Context): VA[JsValue] = {
    val annotations = context.annotations
    ruleProvider(schema, annotations).repath(p => context.path.compose(p)).validate(input)
  }


  private def atArray(schema: QBArray, arr: JsArray, context: Context): VA[JsValue] = {
    val elements: Seq[VA[JsValue]] = arr.value.zipWithIndex.map { case (jsValue, idx) =>
      process(schema.items, jsValue, context.copy(path = context.path \ idx))
    }
    if (elements.exists(_.isFailure)) {
      Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
    } else {
      val updatedArr = JsArray(elements.collect { case Success(js) => js })
      validate(schema, updatedArr, context)
    }
  }

  private def atTuple(schema: QBTuple, array: JsArray, context: Context): VA[JsValue] = {

    val instanceSize = array.value.size
    val schemaSize = schema.qbTypes.size

    val results: Seq[VA[JsValue]] = if (instanceSize > schemaSize) {
      val additionalInstanceValues: Seq[JsValue] = array.value.takeRight(instanceSize - schemaSize)
      val additionalItemsSchema: Option[QBType] = schema.additionalItems
      val result: Seq[VA[JsValue]] = additionalItemsSchema.fold[Seq[VA[JsValue]]](
        Seq(Failure(Seq(context.path -> Seq(ValidationError("Too many items during tuple validation.")))))
      ) {
        case items =>
          val instanceValuesValidated: Seq[VA[JsValue]] = schema.items().zipWithIndex.map { case (item, idx) =>
            process(item, array.value(idx), context.copy(path = context.path \ idx))
          }
          val additionalInstanceValuesValidated: Seq[VA[JsValue]] = additionalInstanceValues.zipWithIndex.map {
            case (jsValue, idx) =>
              process(items, jsValue, context.copy(path = context.path \ idx))
          }
          instanceValuesValidated ++ additionalInstanceValuesValidated
      }
      result
    } else {
      array.value.zipWithIndex.map { case (jsValue, idx) =>
        process(schema.items()(idx), jsValue, context.copy(path = context.path \ idx))
      }
    }

    if (results.exists(_.isFailure)) {
      Failure(results.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
    } else {
      val updatedArr = JsArray(results.collect { case Success(js) => js })
      validate(schema, updatedArr, context)
    }
  }
}