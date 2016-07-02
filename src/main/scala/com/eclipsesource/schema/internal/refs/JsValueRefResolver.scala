package com.eclipsesource.schema.internal.refs

import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.util.Try

object JsValueRefResolver {

  implicit val jsValueRefInstance = new CanHaveRef[JsValue] {
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
  type Errors = ValidationError

  def resolve(path: String, root: JsValue): Either[Errors, JsValue] = {
    val resolver = new JsValueRefResolver
    resolver.resolve(path, new JsValueResolutionScope(root))
  }

  // TODO: what about error reporting, e.g. schema & instance path
//  def resolveRelative(relativePointer: String, resolveFrom: String, obj: JsObject): Either[Errors, JsValue] = {
//
//    def longestContinuousSeq: Either[Errors, (Int, String)] = {
//      val seq = relativePointer.takeWhile(_.isDigit)
//      if (seq.isEmpty) Left(ValidationError("Invalid relative JSON Pointer")) else Right((seq.toInt, relativePointer.drop(seq.length)))
//    }
//
//    def resolveRef(times: Int, current: JsValue): Either[Errors, JsValue] = times match {
//      case 0 => Right(current)
//      case other if current =>
//    }
//
//    for {
//      from <- resolve(resolveFrom, obj).right
//      (idx, rest) <- longestContinuousSeq.right
//    } yield {
//      from
//    }
//  }
}
