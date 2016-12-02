package com.eclipsesource.schema.internal.validators

import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil}
import com.eclipsesource.schema.internal.constraints.Constraints.ArrayConstraints
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsArray, JsValue}

import scalaz.Success

trait ArrayConstraintValidator {

  def validate(json: JsValue, arrayConstraints: ArrayConstraints, resolutionContext: SchemaResolutionContext)
              (implicit lang: Lang): VA[JsValue] = {
    val reader = for {
      minItemsRule <- validateMinItems
      maxItemsRule <- validateMaxItems
      uniqueRule <- validateUniqueness
    } yield { minItemsRule |+| maxItemsRule |+| uniqueRule }
    reader.run((arrayConstraints, resolutionContext)).repath(_.compose(resolutionContext.instancePath)).validate(json)
  }

  def validateMaxItems(implicit lang: Lang): scalaz.Reader[(ArrayConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val maxItems = constraints.maxItems
      Rule.fromMapping {
        case json@JsArray(values) => maxItems match {
          case Some(max) => if (values.size <= max) {
            Success(json)
          } else {
            failure(
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

  def validateMinItems(implicit lang: Lang): scalaz.Reader[(ArrayConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val minItems = constraints.minItems.getOrElse(0)
      Rule.fromMapping {
        case json@JsArray(values) =>
          if (values.size >= minItems) {
            Success(json)
          } else {
            failure(
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

  def validateUniqueness(implicit lang: Lang): scalaz.Reader[(ArrayConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val isUnique = constraints.unique.getOrElse(false)
      Rule.fromMapping {
        case json@JsArray(values) if isUnique =>
          if (values.distinct.size == values.size) {
            Success(json)
          } else {
            failure(
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
    failure(
      Keywords.Any.Type,
      Messages("err.expected.type", "array", SchemaUtil.typeOfAsString(json)),
      context.schemaPath,
      context.instancePath,
      json
    )
}
