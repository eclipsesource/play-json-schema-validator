package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaValidator, SchemaValidator$, SchemaArray}
import com.eclipsesource.schema.internal.{SchemaUtil, Results, Context}
import play.api.data.mapping.{Success, Failure, VA}
import play.api.libs.json.{JsArray, JsValue}


object ArrayValidator extends SchemaTypeValidator[SchemaArray] with ArrayConstraintValidator {

  override def validate(schema: SchemaArray, json: => JsValue, context: Context): VA[JsValue] = json match {
    case JsArray(values) =>
      val elements: Seq[VA[JsValue]] = values.zipWithIndex.map { case (jsValue, idx) =>
        SchemaValidator.process(schema.items, jsValue,
          context.copy(
            schemaPath = context.schemaPath \ idx,
            instancePath = context.instancePath \ idx
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
        context.schemaPath.toString(),
        context.instancePath.toString(),
        context.root,
        json
      )
  }

}
