package com.eclipsesource.schema.internal

import play.api.data.mapping.{Success, Failure}
import play.api.data.validation.ValidationError
import play.api.libs.json.JsUndefined

object ResultAggregator {

  def aggregateResults(validatedProps: Seq[PropertyValidationResult], context: Context): ValidationStatus = {
    validatedProps.foldLeft(ValidationStatus.empty)((status, prop) => prop._2 match {
      case Failure(err) => status.addToInvalidProps(err)
      case Success(JsAbsent) => status
      case Success(undefined: JsUndefined) => status.addToInvalidProps(
        (context.path \ prop._1, Seq(ValidationError(undefined.error)))
      )
      case Success(value) => status.addToValidProps(prop._1 -> value)
    })
  }


}
