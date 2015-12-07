package com.eclipsesource

import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{RefResolver, Results, Context, SchemaUtil}
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import play.api.data.mapping.{Path, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.{Failure => _, Success => _}

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaReads {

  implicit def noValidator[S <: SchemaType] = new SchemaTypeValidator[S] {
    override def validate(schema: S, json: => JsValue, context: Context): VA[JsValue] = Success(json)
  }

  implicit val compoundValidator = CompoundValidator
  implicit val objectValidator = ObjectValidator
  implicit val arrayValidator = ArrayValidator
  implicit val tupleValidator = TupleValidator
  implicit val numberValidator = NumberValidator
  implicit val integerValidator = IntegerValidator
  implicit val stringValidator = StringValidator
  implicit val booleanValidator = noValidator[SchemaBoolean]
  implicit val nullValidator = noValidator[SchemaNull]

  implicit class SchemaTypeExtensionOps[S <: SchemaType](schemaType: S) {

    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)

    private def hasRef(schema: SchemaObject) = schema.properties.collectFirst { case RefAttribute(_, _) => }.isDefined

    def validate(json: => JsValue, context: Context)(implicit validator: SchemaTypeValidator[S]): VA[JsValue] = {
      schemaType match {
        case schema: SchemaObject if hasRef(schema) =>
          val reference = schema.properties.collectFirst { case ref@RefAttribute(_, _) => ref }
          val r = for {
            ref <- reference
            resolved <- RefResolver.resolve(ref.pointer, context)
          } yield resolved
          r match {
            case None =>
              Results.failureWithPath(
              s"Could not resolve ref ${reference.map(_.pointer).getOrElse("")}",
              context.schemaPath,
              context.instancePath,
              json
            )
            case Some(resolved) =>
              Results.merge(
                SchemaValidator.process(resolved, json, context),
                AnyConstraintValidator.validate(json, resolved.constraints.any, context)
              )
          }
        case _ =>
          Results.merge(
            validator.validate(schemaType, json, context),
            AnyConstraintValidator.validate(json, schemaType.constraints.any, context)
          )
      }
    }
  }

  implicit class FailureExtensions(errors: Seq[(Path, Seq[ValidationError])]) {
    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}