package com.eclipsesource.schema.internal.refs

import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.util.Try

object JsonRefResolver {
  implicit val jsValueRefInstance = new CanHaveRef[JsValue] {
    def isResolvable(json: JsValue): Boolean = json match {
      case _: JsObject => true
      case _: JsArray => true
      case _ => false
    }
    def findScopeRefinement(json: JsValue) = None
    def findRef(json: JsValue) = None

    override def resolve(json: JsValue, fragment: String): Either[ValidationError, JsValue] = {
      def fragmentIsInt = Try(fragment.toInt).isSuccess
      json match {
        case obj: JsObject => (obj \ fragment).toEither
        case arr: JsArray if fragmentIsInt => (arr \ fragment.toInt).toEither
        case arr: JsArray => Left(ValidationError(s"Invalid array index $fragment"))
        case _ => Left(ValidationError(s"Fragment $fragment could not be resolved"))
      }
    }
  }

  type JsValueResolutionContext = GenResolutionContext[JsValue]
  type JsValueResolutionScope =  GenResolutionScope[JsValue]
  type JsValueRefResolver = GenRefResolver[JsValue]
}