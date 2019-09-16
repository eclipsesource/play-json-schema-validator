package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.eclipsesource.schema.internal.{Keywords, Results, SchemaUtil, ValidatorMessages}
import com.osinka.i18n.Lang
import play.api.libs.json._
import scalaz.{Failure, Success}

object TupleValidators {

  def validateTuple(additionalItems: Option[SchemaType], schema: SchemaTuple)
                   (implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {


    scalaz.Reader { context =>
      Rule.fromMapping {
        case json@JsArray(values) =>
          val instanceSize = values.size
          val schemaSize = schema.items.size

          val results: Seq[VA[JsValue]] =
            validateItems(instanceSize, schemaSize, additionalItems, schema, values.toSeq, context, json)

          if (results.exists(_.isFailure)) {
            val failures = results.collect { case Failure(err) => err }.reduceLeft(_ ++ _)
              .flatMap(_._2)
            Failure(failures)
          } else {
            val updatedArr = JsArray(results.collect { case Success(js) => js })
            Success(updatedArr)
          }
        case json@other => SchemaUtil.failure(
          Keywords.Any.Type,
          ValidatorMessages("err.expected.type", "array", SchemaUtil.typeOfAsString(other)),
          context.schemaPath,
          context.instancePath,
          json
        )
      }
    }
  }

  def validateItems(instanceSize: Int,
                    schemaSize: Int,
                    additionalItems: Option[SchemaType],
                    schema: SchemaTuple,
                    values: Seq[JsValue],
                    context: SchemaResolutionContext,
                    json: JsValue
                   )(implicit lang: Lang): Seq[VA[JsValue]] = {
    if (schemaSize == 0) {
      values.map(Success(_))
    } else if (instanceSize > schemaSize) {
      val additionalInstanceValues: Seq[JsValue] = values.takeRight(instanceSize - schemaSize)
      val z: Option[Seq[VA[JsValue]]] = additionalItems.map {
        case SchemaValue(JsBoolean(false)) =>
          Seq(
            Results.failureWithPath(
              Keywords.Array.AdditionalItems,
              ValidatorMessages("arr.max", instanceSize, schemaSize),
              context,
              json
            )
          )
        case SchemaValue(JsBoolean(true)) => values.map(Success(_))
        case items =>
          val instanceValuesValidated: Seq[VA[JsValue]] = schema.items.zipWithIndex.map { case (item, idx) =>
            item.validate(
              values(idx),
              context.updateScope(
                _.copy(
                  schemaJsPath = context.schemaPath.map(_ \ idx.toString),
                  instancePath = context.instancePath \ idx.toString
                )
              )
            )
          }
          val additionalInstanceValuesValidated: Seq[VA[JsValue]] = additionalInstanceValues.zipWithIndex.map {
            case (jsValue, idx) =>
              items.validate(
                jsValue,
                context.updateScope(
                  _.copy(
                    schemaJsPath = context.schemaPath.map(_ \ idx.toString),
                    instancePath = context.instancePath \ idx.toString
                  )
                )
              )
          }
          instanceValuesValidated ++ additionalInstanceValuesValidated
      }
      val x: Seq[VA[JsValue]] = values.map(Success(_))
      z.getOrElse(x)
    } else {
      values.zipWithIndex.map { case (jsValue, idx) =>
        schema.items(idx).validate(
          jsValue,
          context.updateScope(
            _.copy(
              schemaJsPath = context.schemaPath.map(p => p \ idx.toString),
              instancePath = context.instancePath \ idx.toString
            )
          )
        )
      }
    }
  }
}

