package com.eclipsesource

import com.eclipsesource.schema.internal.validation.VA
import com.eclipsesource.schema.internal.validators._
import com.eclipsesource.schema.internal.{ResolutionContext, Results, SchemaUtil}
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaWrites}
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.Success

package object schema
  extends SchemaOps
  with JSONSchemaWrites
  with JSONSchemaReads {

  implicit def noValidator[S <: SchemaType] = new SchemaTypeValidator[S] {
    override def validate(schema: S, json: => JsValue, resolutionContext: ResolutionContext): VA[JsValue] = Success(json)
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

    def validate(json: JsValue, resolutionContext: ResolutionContext): VA[JsValue] = {

      (json, schemaType) match {
        case (_, schemaObject: SchemaObject)
          if schemaObject.properties.collectFirst { case r@RefAttribute(path, _) if !resolutionContext.hasBeenVisited(r) => r }.isDefined =>
          val pointer = schemaObject.properties.collectFirst { case r@RefAttribute(path, _) if !resolutionContext.hasBeenVisited(r) => path }
          resolutionContext.refResolver.resolveRefIfAny(resolutionContext.scope)(schemaObject) match {
            case None => Results.failureWithPath(
              s"Could not resolve ref ${pointer.getOrElse("")}",
              resolutionContext.schemaPath,
              resolutionContext.instancePath,
              json)
            case Some(resolved) =>
              val updatedContext = resolutionContext.updateScope(_.copy(schemaPath =  JsPath \ pointer.getOrElse("#")))
              Results.merge(
                resolved.validate(json, updatedContext),
                AnyConstraintValidator.validate(json, resolved.constraints.any, updatedContext)
              )
          }
        case (_: JsObject, schemaObject: SchemaObject) if schemaType.constraints.any.schemaTypeAsString.isDefined =>
          schemaObject.validateA(json, resolutionContext)
        case (_, schemaObject: SchemaObject) if schemaType.constraints.any.schemaTypeAsString.isEmpty =>
          schemaObject.validateA(json, resolutionContext)
        case (_, c: CompoundSchemaType) =>
          c.validateA(json, resolutionContext)
        case (jsArray: JsArray, schemaArray: SchemaArray) if schemaArray.id.isDefined =>
          schemaArray.validateA(jsArray, resolutionContext)
        case (jsArray: JsArray, schemaArray: SchemaArray) =>
          schemaArray.validateA(jsArray, resolutionContext)
        case (jsArray: JsArray, schemaTuple: SchemaTuple) =>
          schemaTuple.validateA(jsArray, resolutionContext)
        case (jsNumber: JsNumber, schemaNumber: SchemaNumber) =>
          schemaNumber.validateA(jsNumber, resolutionContext)
        case (jsNumber: JsNumber, schemaInteger: SchemaInteger) =>
          schemaInteger.validateA(jsNumber, resolutionContext)
        case (jsBoolean: JsBoolean, schemaBoolean: SchemaBoolean) =>
          schemaBoolean.validateA(jsBoolean, resolutionContext)
        case (jsString: JsString, schemaString: SchemaString) =>
          schemaString.validateA(jsString, resolutionContext)
        case (JsNull, schemaNull: SchemaNull) =>
          schemaNull.validateA(json, resolutionContext)
        case (_, _) if schemaType.constraints.any.schemaTypeAsString.isEmpty =>
          Success(json)
        case _ =>
          Results.failureWithPath(s"Wrong type. Expected $schemaType, was ${SchemaUtil.typeOfAsString(json)}.",
            resolutionContext.schemaPath,
            resolutionContext.instancePath,
            json)
      }
    }

    def validateA(json: => JsValue, resolutionContext: ResolutionContext)(implicit validator: SchemaTypeValidator[S]): VA[JsValue] = {
      Results.merge(
        validator.validate(schemaType, json, resolutionContext),
        AnyConstraintValidator.validate(json, schemaType.constraints.any, resolutionContext)
      )
    }
  }

  implicit class FailureExtensions(errors: Seq[(JsPath, Seq[ValidationError])]) {
    def toJson: JsArray = SchemaUtil.toJson(errors)
  }
}
