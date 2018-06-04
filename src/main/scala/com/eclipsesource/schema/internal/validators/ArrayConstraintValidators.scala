package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.validation.Rule
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil}
import com.eclipsesource.schema.{SchemaResolutionContext, SchemaType}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsArray, JsValue}

import scalaz.Success

object ArrayConstraintValidators {

  def validateContains(contains: Option[SchemaType])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = {
    scalaz.Reader { context =>
      Rule.fromMapping {
        json =>
          (json, contains) match {
            case (JsArray(values), Some(containsSchema)) =>
              values.find(value => containsSchema.validate(value, context).isSuccess)
                .map(Success(_))
                .getOrElse(SchemaUtil.failure(
                  "contains",
                  Messages("err.contains"),
                  context.schemaPath.map(_ \ "contains"),
                  context.instancePath,
                  json
                ))
            case (js@JsArray(_), None) => Success(js)
            case (other, _) => expectedArray(other, context)
          }
      }
    }
  }

  def validateMaxItems(max: Option[Int])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      val maxItems = max
      Rule.fromMapping {
        case json@JsArray(values) => maxItems match {
          case Some(max) => if (values.size <= max) {
            Success(json)
          } else {
            SchemaUtil.failure(
              Keywords.Array.MaxItems,
              Messages("arr.max", values.size, max),
              context.schemaPath,
              context.instancePath,
              json
            )
          }
          case None => Success(json)
        }
        case other => expectedArray(other, context)
      }
    }

  def validateMinItems(min: Option[Int])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      val minItems = min.getOrElse(0)
      Rule.fromMapping {
        case json@JsArray(values) =>
          if (values.size >= minItems) {
            Success(json)
          } else {
            SchemaUtil.failure(
              Keywords.Array.MinItems,
              Messages("arr.min", values.size, minItems),
              context.schemaPath,
              context.instancePath,
              json
            )
          }
        case other => expectedArray(other, context)
      }
    }

  def validateUniqueness(unique: Option[Boolean])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      val isUnique = unique.getOrElse(false)
      Rule.fromMapping {
        case json@JsArray(values) if isUnique =>
          if (values.distinct.size == values.size) {
            Success(json)
          } else {
            SchemaUtil.failure(
              Keywords.Array.UniqueItems,
              Messages("arr.dups"),
              context.schemaPath,
              context.instancePath,
              json
            )
          }
        case arr@JsArray(_) => Success(arr)
        case other => expectedArray(other, context)
      }
    }

  private def expectedArray(json: JsValue, context: SchemaResolutionContext)
                           (implicit lang: Lang) =
    SchemaUtil.failure(
      Keywords.Any.Type,
      Messages("err.expected.type", "array", SchemaUtil.typeOfAsString(json)),
      context.schemaPath,
      context.instancePath,
      json
    )
}
