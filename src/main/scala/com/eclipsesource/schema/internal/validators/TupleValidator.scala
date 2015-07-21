package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.data.mapping.{Failure, Success, VA}
import play.api.libs.json.{JsArray, JsValue}

object TupleValidator extends SchemaTypeValidator[SchemaTuple] with ArrayConstraintValidator {

  override def validate(schema: SchemaTuple, json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsArray(values) =>
      val instanceSize = values.size
      val schemaSize = schema.schemaTypes.size

      val results: Seq[VA[JsValue]] = if (instanceSize > schemaSize) {
        val additionalInstanceValues: Seq[JsValue] = values.takeRight(instanceSize - schemaSize)
        val additionalItemsSchema: SchemaType = schema.constraints.additionalItems.getOrElse(SchemaObject())
        additionalItemsSchema match {
          case SchemaBooleanConstant(false) => Seq(Results.failure("Too many items."))
          case SchemaBooleanConstant(true) => values.map(Success(_))
          case items =>
            val instanceValuesValidated: Seq[VA[JsValue]] = schema.items().zipWithIndex.map { case (item, idx) =>
              Validator.process(item, values(idx), context.copy(path = context.path \ idx))
            }
            val additionalInstanceValuesValidated: Seq[VA[JsValue]] = additionalInstanceValues.zipWithIndex.map {
              case (jsValue, idx) =>
                Validator.process(items, jsValue, context.copy(path = context.path \ idx))
            }
            instanceValuesValidated ++ additionalInstanceValuesValidated
        }
      } else {
        values.zipWithIndex.map { case (jsValue, idx) =>
          Validator.process(schema.items()(idx), jsValue, context.copy(path = context.path \ idx))
        }
      }

      if (results.exists(_.isFailure)) {
        Failure(results.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
      } else {
        val updatedArr = JsArray(results.collect { case Success(js) => js })
        validate(updatedArr, schema.constraints)
      }
    case other => Results.failure(s"Expected array, was $other")
  }

}
