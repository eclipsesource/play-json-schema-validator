package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.{SchemaArray, SchemaResolutionContext}
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.{Keywords, Results, SchemaUtil}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsArray, JsValue}

import scalaz.{Failure, Success}


object ArrayValidator extends SchemaTypeValidator[SchemaArray] {

  override def validate(schema: SchemaArray, json: => JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {
    json match {
      case JsArray(values) =>
        val elements: Seq[VA[JsValue]] = values.toSeq.zipWithIndex.map { case (jsValue, idx) =>
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
          schema.constraints.validate(schema, updatedArr, context)
        }
      case _ =>
        Results.failureWithPath(
          Keywords.Any.Type,
          Messages("err.expected.type", SchemaUtil.typeOfAsString(json)),
          context,
          json
        )
    }
  }

}
