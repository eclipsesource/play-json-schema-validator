package com.eclipsesource

import com.eclipsesource.schema.internal.refs._
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{Results, SchemaRefResolver, SchemaUtil}
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.{Failure, Success}

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaReads {

  import SchemaRefResolver._

  implicit def noValidator[S <: SchemaType] = new SchemaTypeValidator[S] {
    override def validate(schema: S, json: => JsValue, resolutionContext: SchemaResolutionContext): VA[JsValue] = Success(json)
  }

  implicit class SchemaTypeExtensionOps[S <: SchemaType](schemaType: S) {

    implicit val compoundValidator = CompoundValidator
    implicit val objectValidator = ObjectValidator
    implicit val arrayValidator = ArrayValidator
    implicit val tupleValidator = TupleValidator
    implicit val numberValidator = NumberValidator
    implicit val integerValidator = IntegerValidator
    implicit val stringValidator = StringValidator
    implicit val booleanValidator = noValidator[SchemaBoolean]
    implicit val nullValidator = noValidator[SchemaNull]

    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)

    def validate(json: JsValue, resolutionContext: SchemaResolutionContext): VA[JsValue] = {

      // refine resolution scope
      val updatedScope: GenResolutionScope[SchemaType] = resolutionContext.refResolver
        .updateResolutionScope(resolutionContext.scope, schemaType)

      val context = resolutionContext.updateScope(_ => updatedScope)

      (json, schemaType) match {

        case (_, schemaObject: SchemaObject) if hasUnvisitedRef(schemaObject, context) =>

          val refValue: Option[String] = context.refResolver.refTypeClass.findRef(schemaObject).map(_._2)
          // TODO: remove get & review
          val p = Pointers.normalize(refValue.map(Pointer).get, schemaObject.constraints.any.id.map(Pointer))
          val fromRoot = refValue.map(Pointer).exists(p => p.isAbsolute || p.isFragment)
          val resolutionRoot =
            if (fromRoot) context.scope.documentRoot
            else schemaObject

          context.refResolver.resolve(resolutionRoot, p, context.scope) match {
            case Left(ValidationError(messages, errors @ _*)) =>
              Results.failureWithPath(
                s"Could not resolve ref ${refValue.orElse(messages.headOption).getOrElse("")}",
                context,
                json)
            case Right(ResolvedResult(resolved, scope)) =>
              val updatedContext  = context.updateScope(_ => scope)
              Results.merge(
                resolved.validate(json, updatedContext),
                AnyConstraintValidator.validate(json, resolved, updatedContext)
              )
          }

        case (_: JsObject, schemaObject: SchemaObject) =>
          schemaObject.validateConstraints(json, context)

        case (_, schemaObject: SchemaObject) if !schemaType.constraints.any.typeGiven =>
          schemaObject.validateConstraints(json,  context)

        case (_, c: CompoundSchemaType) =>
          c.validateConstraints(json, context)

        case (jsArray: JsArray, schemaArray: SchemaArray) =>
          schemaArray.validateConstraints(jsArray, context)

        case (jsArray: JsArray, schemaTuple: SchemaTuple) =>
          schemaTuple.validateConstraints(jsArray, context)

        case (jsNumber: JsNumber, schemaNumber: SchemaNumber) =>
          schemaNumber.validateConstraints(jsNumber, context)

        case (jsNumber: JsNumber, schemaInteger: SchemaInteger) =>
          schemaInteger.validateConstraints(jsNumber, context)

        case (jsBoolean: JsBoolean, schemaBoolean: SchemaBoolean) =>
          schemaBoolean.validateConstraints(jsBoolean, context)

        case (jsString: JsString, schemaString: SchemaString) =>
          schemaString.validateConstraints(jsString, context)

        case (JsNull, schemaNull: SchemaNull) =>
          schemaNull.validateConstraints(json, context)

        case (_, _) if schemaType.constraints.any.schemaTypeAsString.isEmpty =>
          Success(json)

        case _ =>
          Results.failureWithPath(s"Wrong type. Expected $schemaType, was ${SchemaUtil.typeOfAsString(json)}.",
            context,
            json)
      }
    }

    private def hasUnvisitedRef(schemaObject: SchemaObject, resolutionContext: SchemaResolutionContext): Boolean = {
      resolutionContext.refResolver.refTypeClass.findRef(schemaObject)
        .map { case (_, ref) =>
          !resolutionContext.hasBeenVisited(ref) }
        .isDefined
    }

    private[schema] def validateConstraints(json: => JsValue, resolutionContext: SchemaResolutionContext)
                                   (implicit validator: SchemaTypeValidator[S]): VA[JsValue] = {
      Results.merge(
        validator.validate(schemaType, json, resolutionContext),
        AnyConstraintValidator.validate(json, schemaType, resolutionContext)
      )
    }
  }

  implicit class VAExtensions[O](va: VA[O]) {
    def toJsResult: JsResult[O] = va match {
      case Success(s) => JsSuccess(s)
      case Failure(errors) => JsError(errors)
    }
  }

  implicit class FailureExtensions(errors: Seq[(JsPath, Seq[ValidationError])]) {
    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}
