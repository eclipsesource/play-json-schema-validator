package com.eclipsesource.schema.internal.draft7.constraints

import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints.{AnyConstraints, ArrayConstraints, Constraint}
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.validators.TupleValidators
import com.eclipsesource.schema.{SchemaArray, SchemaResolutionContext, SchemaTuple, SchemaType, SchemaValue}
import com.osinka.i18n.Lang
import play.api.libs.json._
import scalaz.Success
import scalaz.std.option._
import scalaz.std.set._
import scalaz.syntax.semigroup._

case class ArrayConstraints7(maxItems: Option[Int] = None,
                             minItems: Option[Int] = None,
                             additionalItems: Option[SchemaType] = None,
                             contains: Option[SchemaType] = None,
                             unique: Option[Boolean] = None,
                             any: AnyConstraints = AnyConstraints7()
                            ) extends ArrayConstraints with Constraint {

  override def subSchemas: Set[SchemaType] =
    (additionalItems.map(Set(_)) |+| contains.map(Set(_))).getOrElse(Set.empty) ++ any.subSchemas


  import com.eclipsesource.schema.internal.validators.ArrayConstraintValidators._

  override def validate(schema: SchemaType, json: JsValue, resolutionContext: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] = {

    val reader = for {
      minItemsRule <- validateMinItems(minItems)
      maxItemsRule <- validateMaxItems(maxItems)
      uniqueRule <- validateUniqueness(unique)
      containsRule <- validateContains(contains)
    } yield {
      minItemsRule |+| maxItemsRule |+| uniqueRule |+| containsRule
    }

    schema match {
      case t: SchemaTuple =>
        TupleValidators
          .validateTuple(additionalItems, t)
          .flatMap(x => reader.map(f => f |+| x))
          .run(resolutionContext)
          .repath(_.compose(resolutionContext.instancePath))
          .validate(json)
      case _: SchemaArray =>
        reader.run(resolutionContext)
          .repath(_.compose(resolutionContext.instancePath))
          .validate(json)
      case _ => Success(json)
    }
  }

  override def resolvePath(path: String): Option[SchemaType] = path match {
    case Keywords.Array.MinItems => minItems.map(min => SchemaValue(JsNumber(min)))
    case Keywords.Array.MaxItems => maxItems.map(max => SchemaValue(JsNumber(max)))
    case Keywords.Array.AdditionalItems => additionalItems
    case Keywords.Array.UniqueItems => unique.map(u => SchemaValue(JsBoolean(u)))
    case "contains" => contains
    case other => any.resolvePath(other)
  }
}