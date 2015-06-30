package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.{Results, Context}
import com.eclipsesource.schema.internal.constraints.Constraints.ArrayConstraints
import play.api.data.mapping.{Failure, Rule, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsArray, JsValue, Json}

object ArrayConstraintValidator {

  def validateTuple(schema: SchemaTuple, json: => JsValue, context: Context): VA[JsValue] = json match {
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

  def validateArray(schema: SchemaArray, json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsArray(values) =>
      val elements: Seq[VA[JsValue]] = values.zipWithIndex.map { case (jsValue, idx) =>
        Validator.process(schema.items, jsValue, context.copy(path = context.path \ idx))
      }
      if (elements.exists(_.isFailure)) {
        Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
      } else {
        val updatedArr = JsArray(elements.collect { case Success(js) => js })
        validate(updatedArr, schema.constraints)
      }
    case other => Results.failure(s"Expected array, was $other")
  }


  def validate(json: JsValue, arrayConstraints: ArrayConstraints): VA[JsValue] = {
    (validateMinItems(arrayConstraints) |+|
      validateMaxItems(arrayConstraints) |+|
      validateUniqueness(arrayConstraints)).validate(json)
  }

  def validateMaxItems(arrayConstraints: ArrayConstraints): Rule[JsValue, JsValue] = {
    val maxItems = arrayConstraints.maxItems
    Rule.fromMapping {
      case json@JsArray(values) => maxItems match {
        case Some(max) => if (values.size <= max) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("maxItems violated",
                Json.obj("maxItems" -> maxItems, "array" -> values)
              )
            )
          )
        }
        case None => Success(json)
      }
      case other => Failure(Seq(ValidationError(s"Expected array, was $other")))
    }
  }

  def validateMinItems(arrayConstraints: ArrayConstraints): Rule[JsValue, JsValue] = {
    val minItems = arrayConstraints.minItems.getOrElse(0)
    Rule.fromMapping {
      case json@JsArray(values) =>
        if (values.size >= minItems) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("minItems violated",
                Json.obj("minItems" -> minItems, "array" -> values)
              )
            )
          )
        }
      case other => Failure(Seq(ValidationError(s"Expected array, was $other")))
    }
  }

  def validateUniqueness(arrayConstraints: ArrayConstraints): Rule[JsValue, JsValue] = {
    val isUnique = arrayConstraints.unique.getOrElse(false)
    Rule.fromMapping {
      case json@JsArray(values) if isUnique =>
        if (values.distinct.size == values.size) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("uniqueItems violated",
                Json.obj("array" -> values)
              )
            )
          )
        }
      case arr@JsArray(_) => Success(arr)
      case other => Failure(Seq(ValidationError(s"Expected array, was $other")))
    }
  }

}
