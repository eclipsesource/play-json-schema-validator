package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{Validator, SchemaArray}
import com.eclipsesource.schema.internal.{Results, Context}
import play.api.data.mapping.{Success, Failure, VA}
import play.api.libs.json.{JsArray, JsValue}


object ArrayValidator extends Validator2[SchemaArray] with ArrayConstraintValidator {

  override def validate(schema: SchemaArray, json: => JsValue, context: Context): VA[JsValue] = json match {
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

}
