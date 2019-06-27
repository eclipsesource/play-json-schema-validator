package com.eclipsesource.schema.internal.draft7.constraints

import com.eclipsesource.schema.{SchemaMap, SchemaObject, SchemaProp, SchemaResolutionContext, SchemaType, SchemaValue}
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints.{AnyConstraints, Constraint, HasAnyConstraint, ObjectConstraints}
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.json.{JsNumber, JsObject, JsValue}
import scalaz.Success
import scalaz.std.option._
import scalaz.std.set._
import scalaz.syntax.semigroup._
import com.eclipsesource.schema.internal._

case class ObjectConstraints7(additionalProps: Option[SchemaType] = None,
                              dependencies: Option[Map[String, SchemaType]] = None,
                              patternProps: Option[Map[String, SchemaType]] = None,
                              required: Option[Seq[String]] = None,
                              minProperties: Option[Int] = None,
                              maxProperties: Option[Int] = None,
                              propertyNames: Option[SchemaType] = None,
                              any: AnyConstraints = AnyConstraints7()
                             ) extends HasAnyConstraint with ObjectConstraints {

  import com.eclipsesource.schema.internal.validators.ObjectValidators._

   override def subSchemas: Set[SchemaType] =
    (additionalProps.map(Set(_)) |+| dependencies.map(_.values.toSet) |+| patternProps.map(_.values.toSet))
      .getOrElse(Set.empty[SchemaType]) ++ any.subSchemas

  override def resolvePath(path: String): Option[SchemaType] = path match {
    case Keywords.Object.AdditionalProperties => additionalProps
    case Keywords.Object.Dependencies => dependencies.map(entries =>
      SchemaMap(Keywords.Object.Dependencies, entries.toSeq.map(e => SchemaProp(e._1, e._2)))
    )
    case Keywords.Object.PatternProperties => patternProps.map(patternProps =>
      SchemaMap(Keywords.Object.PatternProperties, patternProps.toSeq.map(e => SchemaProp(e._1, e._2)))
    )
    case Keywords.Object.MinProperties => minProperties.map(min => SchemaValue(JsNumber(min)))
    case Keywords.Object.MaxProperties => maxProperties.map(max => SchemaValue(JsNumber(max)))
    case "propertyNames" => propertyNames
    case other => any.resolvePath(other)
  }

  override def validate(schema: SchemaType, json: JsValue, context: SchemaResolutionContext)
                       (implicit lang: Lang): VA[JsValue] =
    (schema, json) match {
      case (obj@SchemaObject(_, _, _), jsObject@JsObject(_)) =>
        val validation = for {
          _ <- validateDependencies(schema, dependencies, jsObject)
          remaining <- validateProps(obj.properties, required, jsObject)
          unmatched <- validatePatternProps(patternProps, jsObject.fields.toSeq)
          _ <- validateAdditionalProps(additionalProps, unmatched.intersect(remaining), json)
          _ <- validateMinProperties(minProperties, jsObject)
          _ <- validateMaxProperties(maxProperties, jsObject)
          _ <- validatePropertyNames(propertyNames, jsObject)
        } yield schema

        val (_, _, result) = validation.run(context, Success(json))
        result
      case _ => Success(json)
    }
}
