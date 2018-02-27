package com.eclipsesource.schema.internal.draft7.constraints

import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints.AnyConstraints
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.eclipsesource.schema.{SchemaMap, SchemaProp, SchemaResolutionContext, SchemaSeq, SchemaType, SchemaValue}
import com.osinka.i18n.Lang
import play.api.libs.json._
import scalaz.std.option._
import scalaz.std.set._
import scalaz.syntax.semigroup._

case class AnyConstraints7(schemaType: Option[String] = None,
                           allOf: Option[Seq[SchemaType]] = None,
                           anyOf: Option[Seq[SchemaType]] = None,
                           oneOf: Option[Seq[SchemaType]] = None,
                           definitions: Option[Map[String, SchemaType]] = None,
                           enum: Option[Seq[JsValue]] = None,
                           const: Option[JsValue] = None,
                           not: Option[SchemaType] = None,
                           description: Option[String] = None,
                           id: Option[String] = None,
                           _if: Option[SchemaType] = None,
                           _then: Option[SchemaType] = None,
                           _else: Option[SchemaType] = None
                         )
  extends AnyConstraints {

  override def subSchemas: Set[SchemaType] = {
    // TODO: if then else are missing
    (definitions.map(_.values.toSet) |+| allOf.map(_.toSet) |+| anyOf.map(_.toSet) |+| oneOf.map(_.toSet))
      .getOrElse(Set.empty[SchemaType])
  }

  override def resolvePath(path: String): Option[SchemaType] = path match {
    case Keywords.Any.Type => schemaType.map(t => SchemaValue(JsString(t)))
    case Keywords.Any.AllOf => allOf.map(types => SchemaSeq(types))
    case Keywords.Any.AnyOf => anyOf.map(types => SchemaSeq(types))
    case Keywords.Any.OneOf => oneOf.map(types => SchemaSeq(types))
    case Keywords.Any.Definitions => definitions.map(entries =>
      SchemaMap(
        Keywords.Any.Definitions,
        entries.toSeq.map { case (name, schema) => SchemaProp(name, schema) })
    )
    case Keywords.Any.Enum => enum.map(e => SchemaValue(JsArray(e)))
    case Keywords.Any.Not => not
    case Keywords.Any.If => _if
    case Keywords.Any.Then => _then
    case Keywords.Any.Else => _else
    case "$id" => id.map(id => SchemaValue(JsString(id)))
    case _ => None
  }

  import com.eclipsesource.schema.internal.validators.AnyConstraintValidators._

  override def validate(schema: SchemaType, json: JsValue, context: SchemaResolutionContext)(implicit lang: Lang): VA[JsValue] = {
    val reader: scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] = for {
      allOfRule <- validateAllOf(schema, allOf)
      anyOfRule <- validateAnyOf(schema, anyOf)
      oneOfRule <- validateOneOf(schema, oneOf)
      enumRule <- validateEnum(enum)
      constRule <- validateConst(const)
      notRule <- validateNot(not)
      ifThenElseRule <- validateIfThenElse(_if, _then, _else)
    } yield allOfRule |+| anyOfRule |+| oneOfRule |+| enumRule |+| constRule |+| notRule |+| ifThenElseRule
    reader
      .run(context)
      .repath(_.compose(context.instancePath))
      .validate(json)
  }
}
