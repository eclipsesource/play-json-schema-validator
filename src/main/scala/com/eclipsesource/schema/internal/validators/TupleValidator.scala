package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.data.mapping.{Failure, Success, VA}
import play.api.libs.json.{JsArray, JsValue}

object TupleValidator extends SchemaTypeValidator[SchemaTuple] with ArrayConstraintValidator {

  override def validate(schema: SchemaTuple, json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsArray(values) =>
      val instanceSize = values.size
      val schemaSize = schema.items.size

      val results: Seq[VA[JsValue]] = if (instanceSize > schemaSize) {
        val additionalInstanceValues: Seq[JsValue] = values.takeRight(instanceSize - schemaSize)
        val additionalItemsSchema: SchemaType = schema.constraints.additionalItems.getOrElse(SchemaObject())
        additionalItemsSchema match {
          case SchemaBooleanConstant(false) =>
            Seq(
              Results.failureWithPath(
                s"Too many items. Expected $schemaSize items, found $instanceSize.",
                context.schemaPath,
                context.instancePath,
                json
              )
            )
          case SchemaBooleanConstant(true) =>
            values.map(Success(_))
          case items =>
            val instanceValuesValidated: Seq[VA[JsValue]] = schema.items.zipWithIndex.map { case (item, idx) =>
              SchemaValidator.process(item, values(idx),
                context.copy(
                  schemaPath = context.schemaPath \ idx,
                  instancePath = context.instancePath \ idx
                )
              )
            }
            val additionalInstanceValuesValidated: Seq[VA[JsValue]] = additionalInstanceValues.zipWithIndex.map {
              case (jsValue, idx) =>
                val index = idx + schemaSize
                SchemaValidator.process(items, jsValue,
                  context.copy(
                    schemaPath = context.schemaPath \ index,
                    instancePath = context.instancePath \ index
                  )
                )
            }
            instanceValuesValidated ++ additionalInstanceValuesValidated
        }
      } else {
        values.zipWithIndex.map { case (jsValue, idx) =>
          SchemaValidator.process(schema.items(idx), jsValue,
            context.copy(
              schemaPath = context.schemaPath \ idx,
              instancePath = context.instancePath \ idx
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
      s"Expected array, was $other",
      context.schemaPath,
      context.instancePath,
      json
    )
  }

}
