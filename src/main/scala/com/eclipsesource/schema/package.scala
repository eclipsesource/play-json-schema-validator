package com.eclipsesource

import com.eclipsesource.schema.internal.constraints.Constraints.HasAnyConstraint
import com.eclipsesource.schema.internal.refs._
import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{Keywords, Results, SchemaUtil}
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json._
import scalaz.{-\/, Failure, Success, \/, \/-}

package object schema {

  case class SchemaResolutionContext(refResolver: SchemaRefResolver,
                                     scope: SchemaResolutionScope,
                                     formats: Map[String, SchemaFormat] = DefaultFormats.formats) {

    def schemaPath: JsPath = scope.schemaPath
    def instancePath: JsPath = scope.instancePath
    def updateScope(scopeUpdateFn: SchemaResolutionScope => SchemaResolutionScope): SchemaResolutionContext =
      copy(scope = scopeUpdateFn(scope))

  }

  implicit def noValidator[S <: SchemaType]: SchemaTypeValidator[S] = new SchemaTypeValidator[S] {
    override def validate(schema: S, json: => JsValue, resolutionContext: SchemaResolutionContext)
                         (implicit lang: Lang): VA[JsValue] = Success(json)
  }

  implicit class SchemaTypeExtensionOps[S <: SchemaType](schemaType: S) {

    implicit val compoundValidator: SchemaTypeValidator[CompoundSchemaType] = CompoundValidator
    implicit val objectValidator: SchemaTypeValidator[SchemaObject] = ObjectValidator
    implicit val tupleValidator: SchemaTypeValidator[SchemaTuple] = TupleValidator
    implicit val numberValidator: SchemaTypeValidator[SchemaNumber] = NumberValidator
    implicit val integerValidator: SchemaTypeValidator[SchemaInteger] = IntegerValidator
    implicit val stringValidator: SchemaTypeValidator[SchemaString] = StringValidator
    implicit val arrayValidator: SchemaTypeValidator[SchemaArray] = ArrayValidator
    implicit val booleanValidator: SchemaTypeValidator[SchemaBoolean] = noValidator[SchemaBoolean]
    implicit val nullValidator: SchemaTypeValidator[SchemaNull] = noValidator[SchemaNull]

    def prettyPrint: String = SchemaUtil.prettyPrint(schemaType)

    private def resolveRefAndValidate(json: JsValue, schemaRef: SchemaRef, context: SchemaResolutionContext)
                                     (implicit lang: Lang): VA[JsValue] = {

      context.refResolver.resolve(schemaRef, schemaRef.ref, context.scope) match {
        case -\/(JsonValidationError(_, _*)) =>
          Results.failureWithPath(
            Keywords.Ref,
            Messages("err.unresolved.ref", schemaRef.ref.value),
            context,
            json)
        case \/-(ResolvedResult(resolved, scope)) =>
          val updatedContext = context.updateScope(_ => scope)
          resolved.doValidate(json, updatedContext)
      }
    }

    private[schema] def doValidate(json: JsValue, context: SchemaResolutionContext)(implicit lang: Lang) = {
      (json, schemaType) match {

        case (_, SchemaValue(JsBoolean(false))) =>
          Results.failureWithPath(
            "",
            Messages("err.false.schema", schemaType, SchemaUtil.typeOfAsString(json)),
            context,
            json)

        case (_, SchemaValue(JsBoolean(true))) =>
          Success(json)

        case (_: JsObject, schemaObject: SchemaObject) =>
          schemaObject.validateConstraints(json, context)

        case (_, schemaObject: SchemaObject) if schemaObject.constraints.any.schemaType.isEmpty =>
          schemaObject.validateConstraints(json, context)

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

        case (_, SchemaRoot(_, s)) => s.validate(json, context)

        case (_, s) if s.constraints.schemaType.isEmpty => Success(json)

        case (_, _) =>
          Results.failureWithPath(
            Keywords.Any.Type,
            Messages("err.expected.type", schemaType, SchemaUtil.typeOfAsString(json)),
            context,
            json)
      }
    }

    def validate(json: JsValue, context: SchemaResolutionContext)(implicit lang: Lang): VA[JsValue] = {
      (json, schemaType) match {
        case (_, s@SchemaRef(_, _, _)) => resolveRefAndValidate(json, s, context)
        case _ =>
          // refine resolution scope
          val updatedScope: SchemaResolutionScope = context.refResolver
            .updateResolutionScope(context.scope, schemaType)
          val updateContext = context.updateScope(_ => updatedScope)
          doValidate(json, updateContext)
      }
    }

    private[schema] def validateConstraints(json: => JsValue, resolutionContext: SchemaResolutionContext)
                                           (implicit validator: SchemaTypeValidator[S], lang: Lang): VA[JsValue] = {
      schemaType.constraints match {
        case anyConstraint: HasAnyConstraint =>
          Results.merge(
            validator.validate(schemaType, json, resolutionContext),
            anyConstraint.any.validate(schemaType, json, resolutionContext)
          )
        case _ => validator.validate(schemaType, json, resolutionContext)
      }

    }
  }

  private implicit class SchemaObjectExtension(schemaObject: SchemaObject) {
    def hasRef(resolutionContext: SchemaResolutionContext): Boolean = {
      resolutionContext.refResolver.findRef(schemaObject)
        .isDefined
    }

    def findRef(resolutionContext: SchemaResolutionContext): Option[Ref] =
      resolutionContext.refResolver.findRef(schemaObject)
  }

  implicit class VAExtensions[O](va: VA[O]) {
    def toJsResult: JsResult[O] = va match {
      case Success(s) => JsSuccess(s)
      case Failure(errors) => JsError(errors)
    }
  }

  implicit class ScalazEitherExtensionOps[A](either: \/[JsonValidationError, A]) {
    def toJsResult: JsResult[A] = either match {
      case -\/(errs) => JsError(errs)
      case \/-(schema) => JsSuccess(schema)
    }
  }

  implicit class FailureExtensions(errors: Seq[(JsPath, Seq[JsonValidationError])]) {
    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}
