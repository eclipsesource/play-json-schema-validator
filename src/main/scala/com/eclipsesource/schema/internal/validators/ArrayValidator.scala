package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.SchemaArray
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.{ResolutionContext, Results, SchemaUtil}
import play.api.libs.json.{JsArray, JsValue}

import scalaz.{Failure, Success}


object ArrayValidator extends SchemaTypeValidator[SchemaArray] with ArrayConstraintValidator {

  override def validate(schema: SchemaArray, json: => JsValue, context: ResolutionContext): VA[JsValue] = json match {
    case JsArray(values) =>
      val elements: Seq[VA[JsValue]] = values.zipWithIndex.map { case (jsValue, idx) =>
        schema.item.validate(
          jsValue,
          context.updateScope(
            _.copy(instancePath = context.instancePath \ idx.toString)
          )
        )
      }
      if (elements.exists(_.isFailure)) {
        Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
      } else {
        val updatedArr = JsArray(elements.collect { case Success(js) => js })
        validate(updatedArr, schema.constraints, context)
      }
    case other =>
      Results.failureWithPath(
        s"Wrong type. Expected array, was ${SchemaUtil.typeOfAsString(json)}",
        context.schemaPath,
        context.instancePath,
        json
      )
  }

}
