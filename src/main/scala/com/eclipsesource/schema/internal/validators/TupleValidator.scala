package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.{Keywords, Results, SchemaUtil}
import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsArray, JsBoolean, JsValue}

import scalaz.{Failure, Success}

object TupleValidator extends SchemaTypeValidator[SchemaTuple] with ArrayConstraintValidator {

  override def validate(schema: SchemaTuple, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = json match {
    case JsArray(values) =>
      val instanceSize = values.size
      val schemaSize = schema.items.size

      val results: Seq[VA[JsValue]] = if (instanceSize > schemaSize) {
        val additionalInstanceValues: Seq[JsValue] = values.takeRight(instanceSize - schemaSize)
        val additionalItemsSchema: SchemaType = schema.constraints.additionalItems.getOrElse(SchemaObject())
        additionalItemsSchema match {
          case SchemaValue(JsBoolean(false)) =>
            Seq(
              Results.failureWithPath(
                Keywords.Array.AdditionalItems,
                Messages("arr.max", instanceSize, schemaSize),
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
                    schemaPath = context.schemaPath \ idx.toString,
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
                      schemaPath = context.schemaPath \ idx.toString,
                      instancePath = context.instancePath \ idx.toString
                    )
                  )
                )
            }
            instanceValuesValidated ++ additionalInstanceValuesValidated
        }
      } else {
        values.zipWithIndex.map { case (jsValue, idx) =>
          schema.items(idx).validate(
            jsValue,
            context.updateScope(
              _.copy(
                schemaPath = context.schemaPath \ idx.toString,
                instancePath = context.instancePath \ idx.toString
              )
            )
          )
        }
      }

      if (results.exists(_.isFailure)) {
        Failure(results.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
      } else {
        val updatedArr = JsArray(results.collect { case Success(js) => js })
        validate(updatedArr, schema.constraints, context)
      }

    case other => Results.failureWithPath(
      Keywords.Any.Type,
      Messages("err.expected.type", "array", SchemaUtil.typeOfAsString(other)),
      context,
      json
    )
  }
}
