package com.eclipsesource

import com.eclipsesource.schema.internal.refs._
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{Keywords, Results, SchemaRefResolver, SchemaUtil}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._

import scalaz.{-\/, Failure, Success, \/-}

package object schema
  extends SchemaOps
    with JSONSchemaWrites
    with JSONSchemaReads {

  import SchemaRefResolver._

  implicit def noValidator[S <: SchemaType] = new SchemaTypeValidator[S] {
    override def validate(schema: S, json: => JsValue, resolutionContext: SchemaResolutionContext)
                         (implicit lang: Lang): VA[JsValue] = Success(json)
  }

  implicit class SchemaTypeExtensionOps[S <: SchemaType](schemaType: S) {

    implicit val compoundValidator = CompoundValidator
    implicit val objectValidator = ObjectValidator
    implicit val arrayValidator = ArrayValidator
    implicit val tupleValidator = TupleValidator
    implicit val numberValidator = NumberValidator
    implicit val integerValidator = IntegerValidator
    implicit val stringValidator = StringValidator
    implicit val booleanValidator: SchemaTypeValidator[SchemaBoolean] = noValidator[SchemaBoolean]
    implicit val nullValidator: SchemaTypeValidator[SchemaNull] = noValidator[SchemaNull]

    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)

    private def resolveRefAndValidate(json: JsValue, schemaObject: SchemaObject, context: SchemaResolutionContext)
                                     (implicit lang: Lang): VA[JsValue] = {

      val result: Option[VA[JsValue]] = schemaObject.findUnvisitedRef(context).map { ref =>

        context.refResolver.resolve(schemaObject, ref, context.scope) match {
          case -\/(JsonValidationError(_, _*)) =>
            Results.failureWithPath(
              Keywords.Ref,
              Messages("err.unresolved.ref", ref.value),
              context,
              json)
          case \/-(ResolvedResult(resolved, scope)) =>
            val updatedContext = context.updateScope(_ => scope)
            resolved.doValidate(json, updatedContext)
        }
      }

      result.getOrElse(
        Results.failureWithPath(
          Keywords.Ref,
          Messages("err.unvisited.ref.expected", context.schemaPath),
          context,
          json)
      )
    }

    private[schema] def doValidate(json: JsValue, context: SchemaResolutionContext)(implicit lang: Lang) = {
      (json, schemaType) match {

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
          Results.failureWithPath(
            Keywords.Any.Type,
            Messages("err.expected.type", schemaType, SchemaUtil.typeOfAsString(json)),
            context,
            json)
      }
    }

    def validate(json: JsValue, context: SchemaResolutionContext)(implicit lang: Lang): VA[JsValue] = {
      (json, schemaType) match {
        case (_, schemaObject: SchemaObject) if schemaObject.hasUnvisitedRef(context) =>
          resolveRefAndValidate(json, schemaObject, context)
        case _ =>
          // refine resolution scope
          val updatedScope: GenResolutionScope[SchemaType] = context.refResolver
            .updateResolutionScope(context.scope, schemaType)
          val updateContext = context.updateScope(_ => updatedScope)
          doValidate(json, updateContext)
      }
    }

    private[schema] def validateConstraints(json: => JsValue, resolutionContext: SchemaResolutionContext)
                                           (implicit validator: SchemaTypeValidator[S], lang: Lang): VA[JsValue] =
      Results.merge(
        validator.validate(schemaType, json, resolutionContext),
        AnyConstraintValidator.validate(json, schemaType, resolutionContext)
      )
  }

  private implicit class SchemaObjectExtension(schemaObject: SchemaObject) {
    def hasUnvisitedRef(resolutionContext: SchemaResolutionContext): Boolean = {
      resolutionContext.refResolver.refTypeClass.findRef(schemaObject)
        .map(ref => !resolutionContext.hasBeenVisited(ref))
        .isDefined
    }

    def findUnvisitedRef(resolutionContext: SchemaResolutionContext): Option[Ref] =
      resolutionContext.refResolver.refTypeClass.findRef(schemaObject)
  }

  implicit class VAExtensions[O](va: VA[O]) {
    def toJsResult: JsResult[O] = va match {
      case Success(s) => JsSuccess(s)
      case Failure(errors) => JsError(errors)
    }
  }

  implicit class FailureExtensions(errors: Seq[(JsPath, Seq[JsonValidationError])]) {
    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}
