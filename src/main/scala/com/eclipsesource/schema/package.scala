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

    private def hasRef(schema: SchemaObject) = schema.properties.exists { _.name == "$ref" }

    def validate(json: => JsValue, context: Context)(implicit validator: SchemaTypeValidator[S]): VA[JsValue] = {
      schemaType match {
        case schema: SchemaObject if hasRef(schema) =>
          resolveRef(json, schema, context) match {
            case None => Results.failureWithPath(
              s"Could not resolve ref $schema",
              context.schemaPath.toString(),
              context.instancePath.toString(),
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

    def resolveRef(json: => JsValue, schema: SchemaObject, context: Context): Option[SchemaType] = {
      val reference = schema.properties.collectFirst { case SchemaAttribute("$ref", ref@SchemaRef(_, _, _)) => ref }
      for {
        ref <- reference
        resolved <- RefResolver.resolveRef(ref, context)
      } yield resolved
    }
  }

  implicit class FailureExtensions(errors: Seq[(Path, Seq[ValidationError])]) {
    def toJsError: JsError = {
      // groups errors by path
      val groupedErrors: Seq[(Path, Seq[ValidationError])] = errors.groupBy(_._1).map(x => x._1 -> x._2.flatMap(_._2)).toSeq
      // merge args into top-level
      JsError(groupedErrors.map(e => (JsPath \ e._1.toString(), e._2)))
    }

    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}